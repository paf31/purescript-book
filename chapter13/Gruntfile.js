module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],

    psc: {
      lib: {
	src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      tests: {
        options: {
          module: ["Main"],
          main: true
        },
        src: ["tests/Main.purs", "<%=srcFiles%>"],
        dest: "dist/tests.js"
      }
    },

    execute: {
      tests: {
        src: "dist/tests.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("default", ["psc:lib", "psc:tests", "execute:tests", "dotPsci"]);
};
