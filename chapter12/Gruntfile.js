module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      options: {
        main: "Main",
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
