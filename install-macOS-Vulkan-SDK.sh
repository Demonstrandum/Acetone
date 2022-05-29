#!/bin/sh

# macOS is not too keen on supporting Vulkan, but installing
# this SDK will give good enough support.

# Please take your time to read through this script.

LOCAL="$HOME"/.local

LATEST_SDK_URL="https://sdk.lunarg.com/sdk/download/latest/mac/vulkan-sdk.dmg"
DMG_DEST="$LOCAL"/VulkanSDK.dmg

mkdir -v "$LOCAL"  # Create ~/.local directories if they don't already exist.
mkdir -v "$LOCAL"/lib "$LOCAL"/share "$LOCAL"/bin "$LOCAL"/include

curl "$LATEST_SDK_URL" --output "$DMG_DEST"

# Mount .dmg file
MOUNTED="$(hdiutil attach "$DMG_DEST" | tail -n1)"
DEVICE="$(echo "$MOUNTED" | awk '{ print $1 }')"
VOLUME="$(echo "$MOUNTED" | awk '{ print $NF }')"

INSTALLER="$VOLUME/*.app/Contents/MacOS/InstallVulkan"
VULKAN_TMP="$LOCAL"/VulkanInstall
# Start with a clean install target
rm -fr "$VULKAN_TMP"
# Execute installer app, confirm without user input, and accept license
sudo $INSTALLER in -c --al --am -t "$VULKAN_TMP"
sudo chown -R "$USER" "$VULKAN_TMP"

# Finished extracting, move files to ~/.local folder
cp -a "$VULKAN_TMP"/macOS/lib/*.dylib "$LOCAL"/lib
cp -a "$VULKAN_TMP"/macOS/bin/* "$LOCAL"/bin
cp -a "$VULKAN_TMP"/macOS/share/* "$LOCAL"/share
cp -a "$VULKAN_TMP"/macOS/include/* "$LOCAL"/include

# Detach .dmg
hdiutil detach "$DEVICE"
# Remove Vulkan extraction dest
rm -fr "$VULKAN_TMP"
