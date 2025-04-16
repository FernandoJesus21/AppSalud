//https://stackoverflow.com/questions/48851729/how-can-i-disable-all-action-buttons-while-shiny-is-busy-and-loading-text-is-dis


//Script encargado de deshabilitar los botones de los menus de cambio de gráficos cuando la sesion se encuentra ocupada procesando una carga


$(document).on("shiny:busy", function() {
  var inputs = document.getElementsByTagName("button");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = true;
}
});

$(document).on("shiny:idle", function() {
  var inputs = document.getElementsByTagName("button");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = false;
}
});

