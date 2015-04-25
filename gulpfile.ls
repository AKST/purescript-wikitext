require! {
  'gulp-mocha-phantomjs': mocha-phantom-js 
  'gulp-purescript': purescript
  'gulp-flatten': flatten
  'gulp-concat': concat
  'gulp-clean': clean
  'gulp': gulp
}


########################################################
#                     CONSTANTS                        # 
########################################################

out-dir = "./dist"


mocha-css = "./bower_components/mocha/mocha.css"


html-source = "./test/tests.html"


test-out-name = \tests.js


lib-out-name = \jslibs.js


ps-source = 
  * './bower_components/purescript-*/src/**/*.purs' 
  * './bower_components/purescript-*/src/**/*.purs.hs' 
  * './src/**/*.purs'
  * './test/**/*.purs'


lib-source = 
  * './bower_components/es5-shim/es5-shim.min.js'
  * './bower_components/jquery/dist/jquery.min.js'
  * './bower_components/mocha/mocha.js'
  * './bower_components/chai/chai.js'


########################################################
#                 JAVASCRIPT CONCAT                    # 
########################################################


gulp.task \js-libs ->
  gulp.src lib-source
    .pipe concat lib-out-name
    .pipe gulp.dest out-dir


########################################################
#                  PURESCIPRT BUILD                    # 
########################################################


gulp.task \ps-src-build ->
  gulp.src ps-source
    .pipe purescript.psc do
      browserNamespace: 'Wikitext'
      output: test-out-name
      main: \AllTests
    .on \error (err) !->
      console.error err.message ? err
    .pipe gulp.dest out-dir


########################################################
#                     FILE MOVING                      # 
########################################################


gulp.task \test-html ->
  gulp.src html-source
    .pipe gulp.dest out-dir



gulp.task \3rd-party-css ->
  gulp.src mocha-css
    .pipe gulp.dest out-dir


########################################################
#                    HOUSE CLEANING                    # 
########################################################


gulp.task \clean ->
  gulp.src <[temp public]> { read: false }
    .pipe clean!


########################################################
#                        TESTS                         # 
########################################################


gulp.task \test <[test-html ps-src-build js-libs]> ->
  gulp.src './dist/tests.html'
    .pipe mocha-phantom-js do
      reporter: 'spec'
      mocha: 
        slow: 75


########################################################
#                        WATCH                         # 
########################################################


gulp.task \watch <[test-html 3rd-party-css]> !->
  gulp.watch ps-source, <[test]>
  gulp.watch html-source, [\test-html]



