(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-72801155-2', 'auto');
ga('send', 'pageview');

$(document).on('change', 'input', function(e) {
    ga('send', 'event', 'input', 'input change', $(e.currentTarget).val());
  });
  
$(document).on('click', 'button', function() {
  ga('send', 'event', 'button', 'button click');
});

$(document).on('click', 'a', function() {
  ga('send', 'event', 'button', 'link click');
});