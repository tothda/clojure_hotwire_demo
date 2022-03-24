(ns hotwire
  (:require
    [clojure.core.async :as a]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as str]
    [clojure.test :refer [deftest is run-tests]]
    [clojure.walk]
    [debux.core :refer [dbg dbgn]]
    [hiccup.core :refer [html]]
    [hiccup.page :refer [html5]]
    [reitit.coercion.malli]
    [reitit.core :as r]
    [reitit.ring]
    [reitit.ring.coercion :as rrc]
    [reitit.ring.middleware.exception :as exception]
    [reitit.ring.middleware.multipart :refer [multipart-middleware]]
    [reitit.ring.middleware.parameters :refer [parameters-middleware]]
    [reitit.spec]
    [ring.adapter.jetty :refer [run-jetty]]
    [ring.sse :as sse]
    [ring.middleware.reload :refer [wrap-reload]]
    [ring.util.response :as rr]
    [malli.util]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Domain Logic

(defn init-db []
  (atom {:id-seq 0
         :products {}
         :cart {}}))

(defn save-product! [db {:keys [name price]}]
  (let [{product-id :id-seq} (swap! db update :id-seq inc)]
   (swap! db assoc-in [:products product-id]
     {:id product-id :name name :price price})
    product-id))

(defn find-product
  [db product-id]
  (get-in @db [:products product-id]))

(defn add-product-to-cart!
  [db product-id]
  (swap! db update-in [:cart product-id] (fnil inc 0)))

(defn num-products-in-cart [db]
  (reduce
    (fn [m [k v]] (+ m v))
    0
    (:cart @db)))

(defn num-products-in-cart-for-product [db product-id]
  (get-in @db [:cart product-id] 0))

(defn cart-total-price [db]
  (reduce
    (fn [m [product-id amount]]
      (let [price (get-in @db [:products product-id :price])]
        (+ m (* amount price))))
    0
    (:cart @db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Domain Tests

(deftest test-save-product
  (let [db         (init-db)
        product-id (save-product! db {:name "milk" :price 2.5})]
    (is (= (:name (find-product db product-id)) "milk"))
    (is (= (:price (find-product db product-id)) 2.5))))

(deftest test-add-product-to-cart
  (let [db         (init-db)
        milk-id (save-product! db {:name "milk" :price 2.5})
        beer-id (save-product! db {:name "beer" :price 3.3})]
    (add-product-to-cart! db milk-id)
    (is (= 1 (num-products-in-cart db)))
    (is (= 1 (num-products-in-cart-for-product db milk-id)))
    (add-product-to-cart! db beer-id)
    (add-product-to-cart! db beer-id)
    (is (= 3 (num-products-in-cart db)))
    (is (= 2 (num-products-in-cart-for-product db beer-id)))
    (is (= 9.1 (cart-total-price db)))))

(comment (run-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global State

(defonce app-db nil)

(defn bootstrap-db! []
  (alter-var-root #'app-db (constantly (init-db)))
  (doseq [product
          [{:name "Milk" :price 1.90}
           {:name "Chocolate" :price 2.10}
           {:name "Cheese" :price 4.70}]]
    (save-product! app-db product)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Styles

(def style-button "bg-blue-400 text-white py-1 px-2 rounded")
(def style-link "text-blue-700 underline")
(def style-text-input "px-2 py-1 border rounded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpers

(declare link-to)
(declare render-cart)

(defn page-layout [req children]
  (let [router (get req ::r/router)]
    (list
      [:head
       [:meta {:charset "utf-8"}]
       [:script {:src "/assets/tailwind.js"}]

       [:script {:type "module"}
        "import hotwiredTurbo from 'https://cdn.skypack.dev/@hotwired/turbo';"]
       [:script {:src "/assets/app.js"}]

       ]
      [:body {:class "bg-yellow-50"}
       [:header {:class "bg-orange-700 text-orange-100 flex py-4 sticky top-0 z-50"}
        [:div {:class "px-4"} [:a {:href (link-to router :route-home)} "Home"]]
        [:div {:class "px-4"} [:a {:href (link-to router :route-products)} "Products"]]
        [:div {:class "ml-auto mx-4"}
         (render-cart req)]]
       [:div {:class "container mt-8 mx-auto p-8"}
        children]])))

(defn render-page
  [page-fn req]
  (let [body        (page-fn req)]
    {:body    (html5 (page-layout req body))
     :headers {"Content-Type" "text/html"}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Partial Views (Rails lingo)
;;

(defn product-count-id [product-id] (str "product_count_in_cart_" product-id))

(defn render-product-count-in-cart
  [product-id]
  [:span {:id (product-count-id product-id) :class "w-8 text-right"}
   "x " (num-products-in-cart-for-product app-db product-id)])

(defn render-purchase-event
  [product-id]
  (let [{:keys [name]} (find-product app-db product-id)]
    [:div {:class "pt-2 px-2"}
     [:strong name] " has been purchased."]))

(defn render-cart
  [{:keys [:parameters ::r/router] :as req}]
  (let [product-id (get-in parameters [:form :product-id])]
   [:turbo-frame {:id "cart"}
    [:div {:class "px-3 bg-orange-300 rounded-lg text-orange-800"}
     (format "%s products, CHF %.2f"
       (num-products-in-cart app-db)
       (float (cart-total-price app-db)))]

    ;; Update counter
    (when (some? product-id)
      [:turbo-stream {:action "replace" :target (product-count-id product-id)}
       [:template
        (render-product-count-in-cart product-id)]])

    ; Update cart sidebar
    ; (when (some? product-id)
    ;   [:turbo-stream {:action "append" :target "cart_events"}
    ;    [:template
    ;     (render-purchase-event product-id)]])

    ]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pages

(defn page-home [req]
  [:div {:class "space-y-4"}
   [:h1 {:class "font-bold text-lg"} "Hello World!"]
   [:p "Nulla porttitor accumsan tincidunt. Donec rutrum congue leo eget malesuada. Vivamus suscipit tortor eget felis porttitor volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec velit neque, auctor sit amet aliquam vel, ullamcorper sit amet ligula. Vivamus suscipit tortor eget felis porttitor volutpat."]
   [:p "Mauris blandit aliquet elit, eget tincidunt nibh pulvinar a. Curabitur aliquet quam id dui posuere blandit. Curabitur aliquet quam id dui posuere blandit. Vestibulum ac diam sit amet quam vehicula elementum sed sit amet dui. Donec rutrum congue leo eget malesuada."]
   [:p "Praesent sapien massa, convallis a pellentesque nec, egestas non nisi. Donec sollicitudin molestie malesuada. Curabitur non nulla sit amet nisl tempus convallis quis ac lectus. Cras ultricies ligula sed magna dictum porta. Nulla porttitor accumsan tincidunt."]])


(defn page-new-product
  [{:keys [::r/router]
    :as req}]

  [:turbo-frame {:id "new_product_frame" :target "product_list_frame"}
    [:div
     [:form {:method "post"
             :class "p-4 border space-y-4 bg-white"
             :action (link-to router :route-products)}
      [:h2 {:class "font-bold text-lg"} "Add new product"]
      [:div

       ;; Product name
       [:div
        [:label {:class "text-sm p-2 block font-bold"} "Product name"]
        [:input {:type "text" :name "name" :value "Beer" :class style-text-input}]]

       ;; Price
       [:div
        [:label {:class "text-sm p-2 block font-bold"} "Price"]
        [:input {:type "text" :name "price" :value "3.30" :class style-text-input}]]]

      [:div {:class "mt-4"}

       ;; Save button
       [:button {:class style-button} "Save Product"]

       ;; Cancel link
       [:a {:href (link-to router :route-products)
            :class (str style-link " ml-4")} "cancel"]]]]])

(defn page-products
  [req]
  (let [router (get req ::r/router)]

    [:div {:class "grid grid-cols-3 gap-8"}

     [:div {:class "col-span-2"}

      [:h2 {:class "text-lg font-bold"} "Our products"]

      [:turbo-frame {:id "product_list_frame"}

       ;; Add new product
       [:turbo-frame {:id "new_product_frame" :target "product_list_frame"}
        [:a {:class style-link
             :href (link-to router :route-new-product)
             :data-turbo-frame "_self"
             }
         "Add new product"]]

       ; Product list
       (for [[product-id {:keys [price name]}] (@app-db :products)]
         [:div {:class "pt-4 flex"}
          [:div name]
          [:div {:class "ml-auto w-24 text-right"} price]

          ;; Add button
          [:div {:class "ml-5"}
           [:turbo-frame {:target "cart"}
            [:form {:method "post"
                    :action (link-to router :route-cart)}
              [:input {:type "hidden" :name "product-id" :value product-id}]
              [:button {:class style-button} "Add"]
             ]]]

          ;; Number of products in the cart
          (render-product-count-in-cart product-id)
          ]
         )
       ]]

      ;; Sidebar
      [:div {:id "cart_events" :class "bg-orange-400 text-sm space-y-2 divide-y divide-dashed divide-black"}]

     ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Streaming plumbing
;;

(def stream-response
  (partial assoc {:status 200,
                  :headers {"Content-Type" "text/event-stream"}} :body))

(def EOL "\n")

(defn stream-msg [payload]
  (str "data:" payload EOL EOL))

(def sse-handler
  (sse/event-channel-handler
    (fn [req resp raise ch]
      (let [uuid (java.util.UUID/randomUUID)]
        (swap! app-db assoc-in [:channels uuid] ch)
        uuid))
    {:heartbeat-delay 1
     :on-client-disconnect
     (fn [resp ret-ch]
         (let [uuid (a/<!! ret-ch)
               ch (get-in @app-db [:channels uuid])]
           (swap! app-db update :channels dissoc uuid)
           (println (format "client disconnect, uuid=%s, channel=%s" uuid ch))))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Streaming app logic
;;

(defn render-stream-purchase-event [product-id]
  (html
    [:turbo-stream {:action "append" :target "cart_events"}
     [:template
      (render-purchase-event product-id)]]))

(defn broadcast-cart-event! [product-id]
  (doseq [[uuid ch] (get @app-db :channels)]
    (a/go (a/>! ch
            (stream-msg (render-stream-purchase-event product-id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mutations

(defn handle-save-product!
  [{:keys [:parameters ::r/router :headers]}]
  (let [form-params (get parameters :form)
        {:keys [name price]} form-params]
    (save-product! app-db {:name name :price price})
    (rr/redirect-after-post
      (link-to router :route-products))))

(defn handle-add-product-to-cart!
  [{:keys [:parameters ::r/router] :as req}]
  (let [product-id (get-in parameters [:form :product-id])]
    (add-product-to-cart! app-db product-id)
    (broadcast-cart-event! product-id)
    (render-page render-cart req)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The routing "table"

(defn routes []
  [
   ["/" {:name :route-home
         :handler (fn [req resp raise] (resp (render-page page-home req)))}]

   ["/assets/*" (reitit.ring/create-resource-handler)]

   ["/products"

    ["" {:name :route-products

         :get  {:handler (fn [req resp raise] (resp (render-page page-products req)))}

         :post {:parameters {:form [:map
                                    [:name string?]
                                    [:price number?]]}
                :handler (fn [req resp raise] (resp (handle-save-product! req)))}

         }]

    ["/new" {:name :route-new-product
             :get {:handler (fn [req resp raise]
                              (resp (render-page page-new-product req)))}}]

    ["/cart"
     {:name :route-cart
      :post {:parameters {:form [:map [:product-id integer?]]}
             :handler (fn [req resp raise] (resp (handle-add-product-to-cart! req)))}}]

    ]

   ["/events"
    {:name :route-events
     :handler (fn [req res raise] (sse-handler req res raise))}]

   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HTTP Stack (reitit + malli)

(def exception-middleware
  (exception/create-exception-middleware
    (merge
      exception/default-handlers
      {;; print stack-traces for all exceptions
       ::exception/wrap (fn [error-handler error request]
                          (let [error-resp (error-handler error request)]
                            (println error)
                            (update error-resp :body pr-str)))})))

(defn link-to
  ([router name] (link-to router name nil))
  ([router name params] (link-to router name params nil))
  ([router name params query-params]
   (-> router (r/match-by-name name params) (r/match->path query-params))))

(comment
  ;; To test reverse route generation
  (link-to (router) :route-home)
  (link-to (router) :route-new-product)
  (link-to (router) :route-products))

(defn router []
  (reitit.ring/router
    (routes)
    {:data {:coercion (reitit.coercion.malli/create
                        {;; set of keys to include in error messages
                         :error-keys #{#_:type :coercion :in :schema :value :errors :humanized #_:transformed}
                         ;; schema identity function (default: close all map schemas)
                         :compile malli.util/closed-schema
                         ;; strip-extra-keys (effects only predefined transformers)
                         :strip-extra-keys true
                         ;; add/set default values
                         :default-values true
                         ;; malli options
                         :options nil
                         :encode-error pr-str})
            :middleware [parameters-middleware
                         rrc/coerce-request-middleware
                         rrc/coerce-response-middleware
                         rrc/coerce-exceptions-middleware
                         multipart-middleware]}
     ;; https://cljdoc.org/d/metosin/reitit/0.5.13/doc/basics/route-data-validation
     :validate reitit.spec/validate}))

(def create-handler
  (reitit.ring/ring-handler
    (router)
    (reitit.ring/create-default-handler)
    {:middleware [exception-middleware]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jetty Server

(defonce system (atom {}))

(defn start-jetty! []
  (let [server (run-jetty
                 (wrap-reload #'create-handler) ;; this is sane only in dev environment!
                 {:port   21156
                  :join?  false
                  :async? true})]
    (swap! system assoc :jetty server)))

(defn stop-jetty! []
  (when-some [server (:jetty @system)]
    (.stop server)
    (swap! system dissoc :jetty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Forms to interact with the system

(comment

  (bootstrap-db!)
  (start-jetty!)
  (stop-jetty!)

  ;; Generate some potential sales!
  (a/go-loop [i 20]
    (let [product-id (inc (rand-int (:id-seq @app-db)))]
      (broadcast-cart-event! product-id)
      (a/<! ( a/timeout (* 1000 (+ 1 (rand-int 3)))))
      (when (pos? i)
        (recur (dec i))))))
