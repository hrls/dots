from debian:latest

run apt update
run apt install -y curl zstd lshw

run curl -fsSL https://ollama.com/install.sh | sh
