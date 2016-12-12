(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-72801155-2', 'auto');
ga('send', 'pageview');


$('.shiny-bound-output').on('shiny:error',function(e) {ga('send', 'event', 'error', 'error', $(e.currentTarget).val());});

$(document).on('change', $('#procedure_name'         ), function(e) {ga('send', 'event', 'input', 'procedure_name'   , $(e.currentTarget).val());});
$(document).on('change', $('#measure_selected'       ), function(e) {ga('send', 'event', 'input', 'measure_selected' , $(e.currentTarget).val());});
$(document).on('change', $('#input_method'           ), function(e) {ga('send', 'event', 'input', 'input_method'     , $(e.currentTarget).val());});
$(document).on('change', $('#upload_file'            ), function(e) {ga('send', 'event', 'input', 'upload_file'      , $(e.currentTarget).val());});
$(document).on('change', $('#agree_contribute'       ), function(e) {ga('send', 'event', 'input', 'agree_contribute' , $(e.currentTarget).val());});
$(document).on('change', $('#lab'                    ), function(e) {ga('send', 'event', 'input', 'lab'              , $(e.currentTarget).val());});
$(document).on('change', $('#email'                  ), function(e) {ga('send', 'event', 'input', 'email'            , $(e.currentTarget).val());});
$(document).on('change', $('#mult_adjust'            ), function(e) {ga('send', 'event', 'input', 'mult_adjust'      , $(e.currentTarget).val());});
$(document).on('change', $('#table-metadata input'   ), function(e) {ga('send', 'event', 'input', 'table-metadata'   , $(e.currentTarget).val());});
$(document).on('change', $('#table-gxl input'        ), function(e) {ga('send', 'event', 'input', 'table-gxl'        , $(e.currentTarget).val());});
$(document).on('change', $('#table-groups-info input'), function(e) {ga('send', 'event', 'input', 'table-groups-info', $(e.currentTarget).val());});


$(document).on('click', $('#reset_all')          , function() {ga('send', 'event', 'button', 'reset_all');});
$(document).on('click', $('#load_example_button'), function() {ga('send', 'event', 'button', 'load_example_button');});
$(document).on('click', $('#read_more')          , function() {ga('send', 'event', 'button', 'read_more');});
$(document).on('click', 'a'                      , function() {ga('send', 'event', 'button', 'link click');});
