module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    psc: {
      options: {
        modules: ["Data.PhoneBook"]
      },
      all: {
	src: ["src/**/*.purs", "bower_components/**/src/**/*.purs"],
        dest: "dist/Main.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all"]);
};
