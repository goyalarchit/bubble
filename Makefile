.ONESHELL:

ifndef CI_PROJECT_NAME
override baseurl = public
else
override baseurl = $(CI_PROJECT_NAME)
endif

ifndef S3_BUCKET_NAME
override S3_BUCKET_NAME = "vx.cemca.org"
endif

init:
	mkdir public

build_html:
	# cp ./index.md algodynamics-theme/
	cp -r ./_experiments algodynamics-theme/
	cp -r assets/* algodynamics-theme/assets/
	cd algodynamics-theme
	npm install
	$(shell pwd)
	bundle install
	NODE_ENV=production JEKYLL_ENV=production bundle exec jekyll build -b $(baseurl) --trace
	cp -r ./_site/* ../public/
	cd ..

build_apps:
	mkdir -p ./public/js
	@if [ -z "$(shell which elm)" ]; then\
		@echo "Elm Does not exist";\
		npm install -g elm@latest-0.19.1 --unsafe-perm=true;\
	fi
	@echo "Compiling Elm Files"
	elm make src/BubbleSortTest.elm --output public/js/bubbleSortTest.js
	elm make src/HeapifyTest.elm --output public/js/heapifyTest.js
	elm make src/HeapSortTest.elm --output public/js/heapSortTest.js

all:	init build_html build_apps

clean:
	rm -rf ./public

rebuild:	clean all

deploy_aws:
	@echo "running aws_prod_deploy_job"
	pip install awscli
	aws s3 cp ./public/ s3://$(S3_BUCKET_NAME)/$(CI_PROJECT_NAME)/ --recursive 
	@echo "aws_prod_deploy_job completed"

deploy_gh_pages:
	@echo "running gh_pages_prod_deploy_job"

deploy_all:	deploy_aws deploy_gh_pages