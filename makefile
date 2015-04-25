WATCH=find . -name '*.hs' | entr
GULP=gulp --require=LiveScript


define when-dependency-missing 
	@echo 'CHECKING FOR DEPENDENCY $1' 
	@command -v $1 >/dev/null 2>&1 || $2
endef


define npm-check
	$(call when-dependency-missing, $1, npm install -g $2)
endef


watch:
	${GULP} watch

test:
	${GULP} test 
	
init:
	$(call npm-check, bower, bower)
	$(call npm-check, gulp,  gulp)
	$(call npm-check, lsc,   LiveScript)
	mkdir dist
	npm install
	bower install
  
.PHONY: test watch init
