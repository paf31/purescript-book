module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: [
      "bower_components/**/src/**/*.purs",
      "../dependencies/Control/Monad/Eff/DOM.purs"
    ],

    psc: {
      options: {
        modules: ["Main"]
      },
      rectangle: {
      src: ["src/Rectangle.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      shapes: {
      src: ["src/Shapes.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      random: {
      src: ["src/Random.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      refs: {
      src: ["src/Refs.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      lsystem: {
      src: ["src/LSystem.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("rectangle", ["psc:rectangle"]);
  grunt.registerTask("shapes",    ["psc:shapes"]);
  grunt.registerTask("random",    ["psc:random"]);
  grunt.registerTask("refs",      ["psc:refs"]);
  grunt.registerTask("lsystem",   ["psc:lsystem"]);
  grunt.registerTask("default",   ["psc:rectangle", "dotPsci"]);
};
