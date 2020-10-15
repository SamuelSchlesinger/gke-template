CURL_CMD="curl -sS"
INGRESS_IP="$(kubectl get svc server -o json  | jq '.status.loadBalancer.ingress[0].ip' | sed 's/^"//' | sed 's/"$//')"
$CURL_CMD http://$INGRESS_IP/health
echo "Users: $($CURL_CMD http://$INGRESS_IP)"
NEW_USER=$(cat /dev/urandom | env LC_CTYPE=C tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1)
$CURL_CMD http://$INGRESS_IP -H 'Content-Type: application/json' --data-ascii "\"$NEW_USER\""
echo "Users after inserting \"$NEW_USER\": $($CURL_CMD http://$INGRESS_IP)"
