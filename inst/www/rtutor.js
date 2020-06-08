$(document).on('click','.nextExBtn',function(e){
  var id = e.currentTarget.id;
  var curtab  = parseInt(id.substring(9));
  var nexttab = curtab+1;

  var tabsetid = $("#exTabsetPanel").data("tabsetid");

  $("#exTabsetPanel li:nth-child("+curtab+")").removeClass('active');
  $("#exTabsetPanel li:nth-child("+nexttab+")").addClass('active');

  $("#tab-"+tabsetid+"-"+curtab).removeClass('active');
  $("#tab-"+tabsetid+"-"+nexttab).addClass('active');

  $(window).scrollTop(0);
  //alert('You clicked on the nextExBtn ' + curtab);
});