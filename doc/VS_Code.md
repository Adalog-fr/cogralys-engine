# Run ATGDB inside the VS Code container

```bash
NEO4J_HOST="http://host.docker.internal:7474/db/neo4j/tx/commit" /workspaces/Asis_To_GraphDB/bin_linux/atgdb -dvx t_usage.adb 2> atgdb-trace.txt
```

# Performance issues with container
- https://code.visualstudio.com/remote/advancedcontainers/improve-performance
- https://code.visualstudio.com/docs/remote/create-dev-container#_extend-your-docker-compose-file-for-development