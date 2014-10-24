module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: [
      "src/**/*.purs", 
      "../chapter7/src/**/*.purs", 
      "bower_components/**/src/**/*.purs",
      "../dependencies/Control/Monad/Eff/DOM.purs"
    ],

    psc: {
      options: {
        modules: ["Main"]
      },
      all: {
      src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all", "dotPsci"]);
};
