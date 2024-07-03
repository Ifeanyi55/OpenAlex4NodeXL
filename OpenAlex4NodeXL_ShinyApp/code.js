// query key press action on keyboard
document
    .addEventListener("keydown",function(event){
    // if the user presses the "Enter" key on the keyboard
    if(event.key === "Enter"){
        // cancel the default action if any
        event.preventDefault();
        // trigger the button element with a click
        document.getElementById("query").click();

    }
});