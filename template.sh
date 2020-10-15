mkdir scripts

cat <<EOF > scripts/new-cluster
#!/bin/bash
gcloud auth login
gcloud auth configure-docker
gcloud config set project $GCLOUD_PROJECT_NAME
gcloud container clusters create $CLUSTER_NAME
EOF

chmod +x scripts/new-cluster

cat <<EOF > scripts/deploy
#!/bin/bash
kubectl apply -f k8s/server
kubectl apply -f k8s/postgres
kubectl apply -f k8s/redis
EOF

chmod +x scripts/deploy

cat <<EOF > scripts/build
#!/bin/bash
cd server/builder
docker build . -t $DOCKER_REGISTRY/server-builder
docker push $DOCKER_REGISTRY/server-builder
cd -

cd server/runner
docker build . -t $DOCKER_REGISTRY/server-runner
docker push us.gcr.io/haskell-learn/server-runner
cd -

cd server/hs
docker build . -t $DOCKER_REGISTRY/server-common
docker push $DOCKER_REGISTRY/server-common
cd -

cd server/hs
docker build . -t $DOCKER_REGISTRY/server --file main/Dockerfile
docker push $DOCKER_REGISTRY/server
cd -

cd server/hs
docker build . -t $DOCKER_REGISTRY/migrate --file migrate/Dockerfile
docker push $DOCKER_REGISTRY/migrate
cd -

cd server/hs
docker build . -t $DOCKER_REGISTRY/fuzzer --file fuzzer/Dockerfile
docker push $DOCKER_REGISTRY/fuzzer
cd -
EOF

chmod +x scripts/build

cat <<EOF > scripts/teardown
#!/bin/bash
kubectl delete -f redis/k8s
kubectl delete -f postgres/k8s
kubectl delete -f server/k8s
EOF

mkdir k8s

mkdir k8s/postgres

cat <<EOF > k8s/postgres/pvc.yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: postgres-pvc
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 500Gi
EOF

cat <<EOF > k8s/postgres/config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: postgres-config
  labels:
    app: learning
data:
  postgres.conf: |
    include_if_exists '/var/lib/postgresql/data/postgresql.conf'
    shared_preload_libraries = 'pg_stat_statements'
    max_connections = 150
  password: password
EOF

cat <<EOF > k8s/postgres/service.yaml
apiVersion: v1
kind: Service
metadata:
  name: postgres
spec:
  selector:
    app: learning
  type: ClusterIP
  clusterIP: "None"
  ports:
  - name: postgres
    protocol: TCP
    port: 5432
    targetPort: 5432
EOF

cat <<EOF k8s/postgres/statefulset.yaml
kind: StatefulSet
apiVersion: apps/v1
metadata:
  name: postgres
  labels:
    app: learning
spec:
  serviceName: postgres
  replicas: 1
  revisionHistoryLimit: 10
  selector:
    matchLabels:
      app: learning
  template:
    metadata:
      labels:
        app: learning
    spec:
      terminationGracePeriodSeconds: 380
      volumes:
      - name: postgres-conf
        configMap:
          name: postgres-config
          items:
          - key: postgres.conf
            path: postgres.conf
      - name: muh-data
        persistentVolumeClaim:
          claimName: postgres-pvc
      containers:
      - name: postgres
        image: postgres:latest
        imagePullPolicy: Always
        volumeMounts:
          - mountPath: /data/db
            name: muh-data
          - mountPath: /postgres-conf
            name: postgres-conf
        ports:
        - containerPort: 5432
        env:
          - name: POSTGRES_PASSWORD
            valueFrom:
              configMapKeyRef:
                name: postgres-config
                key: password
EOF

mkdir k8s/redis

cat <<EOF k8s/redis/pvc.yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: redis-pvc
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 500Gi
EOF

cat <<EOF k8s/redis/config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: redis-config
  labels:
    app: learning
data:
  redisPassword: "password"
  redis.conf: |
    requirepass "password"
    port 6379
EOF

cat <<EOF k8s/redis/service.yaml
apiVersion: v1
kind: Service
metadata:
  name: redis
spec:
  selector:
    app: learning
  type: ClusterIP
  clusterIP: "None"
  ports:
  - name: postgres
    protocol: TCP
    port: 6379
    targetPort: 6379
EOF

cat <<EOF k8s/redis/statefulset.yaml
kind: StatefulSet
apiVersion: apps/v1
metadata:
  name: redis
  labels:
    app: learning
spec:
  serviceName: redis
  replicas: 1
  revisionHistoryLimit: 10
  selector:
    matchLabels:
      app: learning
  template:
    metadata:
      labels:
        app: learning
    spec:
      terminationGracePeriodSeconds: 380
      volumes:
      - name: redis-conf
        configMap:
          name: redis-config
          items:
          - key: redis.conf
            path: redis.conf
      - name: muh-data
        persistentVolumeClaim:
          claimName: redis-pvc
      containers:
      - name: redis
        image: redis:latest
        imagePullPolicy: Always
        command:
          - redis-server
          - "/redis.conf/redis.conf"
        volumeMounts:
          - mountPath: /redis-master-data
            name: muh-data
          - mountPath: /redis.conf
            name: redis-conf
        ports:
        - containerPort: 6379
EOF

mkdir hs


