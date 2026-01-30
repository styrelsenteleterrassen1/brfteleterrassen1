#!/usr/bin/env bash

set -e

PORT="${1:-8000}"

echo "Starting HTTP server on port $PORT..."
echo "Serving from: web/"
echo "Open http://localhost:$PORT in your browser"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

cd web && python3 -m http.server "$PORT"
