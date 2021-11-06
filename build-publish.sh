docker pull 2123io/hs-deps:latest || true

docker build --target dependencies --cache-from 2123io/hs-deps:latest -t 2123io/hs-deps:latest .

docker build --target app --cache-from 2123io/hs-deps:latest -t 2123io/fulgurite:latest .

docker push 2123io/hs-deps:latest
docker push 2123io/fulgurite:latest
