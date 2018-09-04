window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'UA-125099925-2');
$('#sidebarItemExpanded > ul').on('click', 'a', function(elem) {
  let screenName = $(this).text().trim();
  gtag('event', 'screen_view', {
    'app_name': 'HIV Estimates Accuracy',
    'screen_name': screenName
  });
});
