window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'UA-125099925-2', {
  'app_name': 'HIV Estimates Accuracy'
});
$(function() {

  gtag('event', 'page_loaded', {
    'event_category': 'screen_change',
    'event_label': 'Input data upload'
  });

  $('#sidebarItemExpanded > ul').on('click', 'a', function(elem) {
    let screenName = $(this).text().trim();
    gtag('event', 'click', {
      'event_category': 'screen_change',
      'event_label': screenName
    });
  });
});

