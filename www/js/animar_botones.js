//https://stackoverflow.com/questions/48851729/how-can-i-disable-all-action-buttons-while-shiny-is-busy-and-loading-text-is-dis


//script que permite las animaciones de los botones de las secciones
var animateButton = function(e) {

  e.preventDefault;
  //reset animation
  e.target.classList.remove('animate');
  
  e.target.classList.add('animate');
  setTimeout(function(){
	e.target.classList.remove('animate');
  },700);
};

var bubblyButtons = document.getElementsByClassName("tabbable");
var bubblyButtons2 = document.getElementsById("summary_tab");

for (var i = 0; i < bubblyButtons.length; i++) {
  bubblyButtons[i].addEventListener('click', animateButton, false);
}
bubblyButtons2.addEventListener('click', animateButton, false);