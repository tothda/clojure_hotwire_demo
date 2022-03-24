window.addEventListener("DOMContentLoaded", (event) => {
    let source = new EventSource("/events");
    Turbo.session.connectStreamSource(source);
});
