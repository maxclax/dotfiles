#!/usr/bin/env bash

# Check if 1Password CLI is installed
if ! command -v op &>/dev/null; then
	echo "Error: 1Password CLI is not installed. Please install it first."
	echo "Visit: https://1password.com/downloads/command-line/"
	return 1
fi

# Function to safely export a secret from 1Password
# Usage: export_op_secret ENV_VAR_NAME "op://Vault/Item/field"
export_op_secret() {
	local env_var_name="$1"
	local op_path="$2"

	if [[ -z "$env_var_name" || -z "$op_path" ]]; then
		echo "Error: Missing parameters for export_op_secret"
		echo "Usage: export_op_secret ENV_VAR_NAME \"op://Vault/Item/field\""
		return 1
	fi

	local secret_value
	secret_value=$(op read "$op_path" 2>/dev/null)

	if [[ $? -ne 0 ]]; then
		echo "Error: Failed to retrieve secret from 1Password: $op_path"
		echo "Make sure you're signed in to 1Password CLI (run 'op signin')"
		return 1
	fi

	export "$env_var_name"="$secret_value"
	echo "✓ Exported $env_var_name"
}

# Export all API keys and sensitive data
export_all_secrets() {
	echo "Exporting secrets from 1Password..."

	# Add more secrets as needed:
	# export_op_secret GITHUB_TOKEN "op://Private/GitHub/token"

	echo "Done exporting secrets."
}

# Execute the function if this file is sourced
export_all_secrets
