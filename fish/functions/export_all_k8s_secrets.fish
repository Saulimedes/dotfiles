function export_all_k8s_secrets
    kubectl get secrets --all-namespaces -o custom-columns="NS:.metadata.namespace,NAME:.metadata.name" --no-headers | while read -l ns secret_name
        set outfile "$ns"_"$secret_name".yaml
        kubectl get secret -n $ns $secret_name -o yaml > $outfile
        echo "Exported $outfile"
    end
end
