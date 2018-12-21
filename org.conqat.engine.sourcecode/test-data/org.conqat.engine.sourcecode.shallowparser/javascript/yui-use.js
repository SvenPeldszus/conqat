YUI().use('node', function(Y) {
    var count = 6, // seconds for timer count down
        changer = Y.one('.changer'), // just the node to change
        counters = Y.all('.fruit .count'); // counter numbers on both fruits

    var doChange = function(){
        count -= 1;  // decrement the seconds count
        counters.setHTML(count); // update the number of seconds by setting the contents of the node

        // when it gets to 0 seconds, make changes to just the node with class "changer"
        if(count < 1){
            // set the "src" attribute of one image to an orange
            Y.one('.changer img').setAttribute('src', '../assets/yui/images/orange.png');

            // change the contents of one speech bubble
            Y.one('.changer .speech').setHTML("No, I'm from <em>Florida!</em>");

            // change several styles together
            changer.setStyles({
                fontSize: '150%',
                color: '#EA6500',
                backgroundColor: '#FFD7AA'
            });
            return; // bypass the setTimeout and don't call the function again
        }
        setTimeout(doChange, 1000);
    }
    doChange();
});
