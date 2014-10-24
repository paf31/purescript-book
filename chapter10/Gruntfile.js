module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: [
      "src/**/*.purs",
      "../chapter7/src/**/*.purs",
      "../chapter8/src/**/Data/AddressBook/UI.purs",
      "bower_components/**/src/**/*.purs",
      "../dependencies/Control/Monad/Eff/DOM.purs"
    ],

    psc: {
      options: {
        modules: ["Main"]
      },
      all: {
    src: ["<%=srcFiles%>"],
        dest: "html/dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"],

    connect: {
      html: {
        options: {
          base: "html",
          keepalive: true
        } 
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-contrib-connect");
  grunt.registerTask("default", ["psc:all", "dotPsci", "connect:html"]);
};
