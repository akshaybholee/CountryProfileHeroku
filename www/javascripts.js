//screen resolution adjustment

$(document).on('shiny:connected', function(e) {
var w = window.screen.width;
var h = window.screen.height;

const str = w + '' + h;


if (str == 1366768)
{
document.body.style.setProperty('zoom', '0.70');
document.body.style.setProperty('background-color', '#ecf0f5');}
else if (str == 14001050)
  {document.body.style.setProperty('zoom', '0.75');
document.body.style.setProperty('background-color', '#ecf0f5');}
else if (str == 1440900)
  {document.body.style.setProperty('zoom', '0.75');
document.body.style.setProperty('background-color', '#ecf0f5');}
else if (str == 1600900)
  {document.body.style.setProperty('zoom', '0.77');
document.body.style.setProperty('background-color', '#ecf0f5');}
else if (str == 16801050)
  {document.body.style.setProperty('zoom', '0.90');
document.body.style.setProperty('background-color', '#ecf0f5');}
else
{document.body.style.setProperty('background-color', '#ecf0f5');}

                            })