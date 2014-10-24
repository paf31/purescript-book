module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      options: {
        modules: ["Data.AddressBook", "Data.AddressBook.Validation"]
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
