$(document).on('click','.nextExBtn',function(e){
  
/*
  // Unfortunately in the pure java script solution below
  // Code chunks will not always be shown in the new tab
  // So I just use the javascript code to scroll up
  // and change the tabset via a buttonHandler in R
  
  var id = e.currentTarget.id;
  var curtab  = parseInt(id.substring(9));
  var nexttab = curtab+1;
  
  Shiny.setInputValue("exTabsetPanel", "exPanel"+nexttab, {priority: "event"});

  var tabsetid = $("#exTabsetPanel").data("tabsetid");
  $("#exTabsetPanel li:nth-child("+curtab+")").removeClass('active');
  $("#exTabsetPanel li:nth-child("+nexttab+")").addClass('active');

  $("#tab-"+tabsetid+"-"+curtab).removeClass('active');
  $("#tab-"+tabsetid+"-"+nexttab).addClass('active');
*/

  $(window).scrollTop(0);
  //alert('You clicked on the nextExBtn ' + curtab);
});