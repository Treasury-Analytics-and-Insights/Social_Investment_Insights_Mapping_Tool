  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  // ga('create', 'UA-73784872-1', 'auto'); // Edit for your own project
  ga('send', 'pageview');
  
  //tracks map region clicks
$('#map').bind('DOMSubtreeModified',function(){ 
                $(this).find('g').on('click',function(){
                                var leaflet = $('.leaflet-popup-content'); 
        var label = leaflet.html().split('<br>')[0]; 
                                ga('send','event','map','check region stats',label);
                })                                             
});


//tracks map filter changes
$('select').on('change',function(){
                var selectedValue = $(this).val();
                ga('send','event','filters','filter change',selectedValue);
});

//tracks export click
$('#downloadDataA').on('click',function(){
                ga('send','event','button','export data','map data');
});
