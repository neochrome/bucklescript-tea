// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Vdom = require("../src-ocaml/vdom.js");
var Block = require("bs-platform/lib/js/block.js");
var Tea_app = require("../src-ocaml/tea_app.js");
var Tea_html = require("../src-ocaml/tea_html.js");

function update$prime(model, param) {
  return /* tuple */[
          model[0],
          "right"
        ];
}

function render_model(param) {
  var exit = 0;
  if (param[0] !== undefined && param[1] !== undefined) {
    return Tea_html.input$prime(undefined, undefined, /* :: */[
                /* RawProp */Block.__(0, [
                    "value",
                    "This should be on screen"
                  ]),
                /* [] */0
              ], /* [] */0);
  } else {
    exit = 1;
  }
  if (exit === 1) {
    return Tea_html.span(undefined, undefined, /* [] */0, /* :: */[
                /* Text */Block.__(1, ["nothing"]),
                /* [] */0
              ]);
  }
  
}

function view$prime(model) {
  return Tea_html.div(undefined, undefined, /* [] */0, /* :: */[
              Tea_html.button(undefined, undefined, /* :: */[
                    Vdom.onMsg("click", /* Trigger */0),
                    /* [] */0
                  ], /* :: */[
                    /* Text */Block.__(1, ["trigger rerender"]),
                    /* [] */0
                  ]),
              /* :: */[
                render_model(model),
                /* [] */0
              ]
            ]);
}

var partial_arg_000 = /* model : tuple */[
  "left",
  undefined
];

var partial_arg = /* record */[
  partial_arg_000,
  /* update */update$prime,
  /* view */view$prime
];

function main(param, param$1) {
  return Tea_app.beginnerProgram(partial_arg, param, param$1);
}

var trigger = /* Trigger */0;

exports.trigger = trigger;
exports.update$prime = update$prime;
exports.render_model = render_model;
exports.view$prime = view$prime;
exports.main = main;
/* Tea_html Not a pure module */
