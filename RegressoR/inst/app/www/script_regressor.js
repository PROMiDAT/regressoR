var promidat_flat_models = {knn: true, dt: true, rf: true, 
                            boosting: true, svm: true, rl: true, 
                            nn: true, rlr: true, rd: true};

nya_btn = true;

function nya_btn_fun(e){
  nya_btn = nya_btn?($("#distribuciones_ui_1-distribucion_numerica").click(),false):true;
}

function promidat_model_firt(e, model, id){
  if(promidat_flat_models[model]){
    promidat_flat_models[model] = false;
    $("#"+id).click();
  }
}

 window.addEventListener("load", function(event) {
  /* Al cargarse la página  se pone el titulo */
  $("header").find("nav").append('<span class="header-title"> <i>regresso</i>R </span>');

  /* Los modelos se vuelven ejecutar al ser seleccionados una vez se haga una segmentacion diferente*/
  $("a[href^='#shiny-tab-knn']").on('click', function(e){promidat_model_firt(e,"knn","runKnn")});
  $("a[href^='#shiny-tab-dt']").on('click', function(e){promidat_model_firt(e,"dt","runDt")});
  $("a[href^='#shiny-tab-rf']").on('click', function(e){promidat_model_firt(e,"rf","runRf")});
  $("a[href^='#shiny-tab-boosting']").on('click', function(e){promidat_model_firt(e,"boosting","runBoosting")});
  $("a[href^='#shiny-tab-svm']").on('click', function(e){promidat_model_firt(e,"svm","runSvm")});
  $("a[href^='#shiny-tab-nn']").on('click', function(e){promidat_model_firt(e,"nn","runNn")});
  $("a[href$='#shiny-tab-rl']").on('click', function(e){promidat_model_firt(e,"rl","linear_regression_ui_1-runRl")});
  $("a[href^='#shiny-tab-rlr']").on('click', function(e){promidat_model_firt(e,"rlr","runRlr")});
  $("a[href^='#shiny-tab-rd']").on('click', function(e){promidat_model_firt(e,"rd","runRd")});
  $("a[data-value='predicModelo']").on('click', function(e){ $("#predecirPromidat").click()});
  $($($($("#distribuciones_ui_1-tabDyA").next().children()[2]).children()[4]).children()[2]).on('click', nya_btn_fun)
  $("#load_data_ui_1-segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, rl: true, nn: true, rlr: true, rd:true};
  });
});


function eliminar_tabs_extras(){
  $("ul#normal_ui_1-BoxNormal li")[2].remove();
  $("ul#dispersion_ui_1-BoxDisp li")[1].remove();
  $("ul#distribuciones_ui_1-tabDyA li")[2].remove();
  $("ul#correlacion_ui_1-tabCor li").last().remove();
  $("ul#BoxKnn li").last().remove();
  $("ul#BoxDt li").last().remove();
  $("ul#BoxRf li").last().remove();
  $("ul#BoxB li").last().remove();
  $("ul#BoxSvm li").last().remove();
  $("ul#model_comparison_ui_1-BoxCom li").last().remove();
  $("ul#BoxModelo li").last().remove();
  $("ul#BoxModelo li").last().remove();
  $("ul#BoxPodPred li").last().remove();
  $("ul#BoxPodPred li").last().remove();
  $("ul#BoxNn li").last().remove();
  $("ul#linear_regression_ui_1-BoxRl li").last().remove();
  $("ul#BoxRd li").last().remove();
}


Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]")
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
      if(message.ids[i] == "categorico" || message.ids[i] == "numerico" || message.ids[i] == "disyuntivo"){
        $("option[value='"+message.ids[i]+"']").text(message.values[i])
      }
    }
  }
);