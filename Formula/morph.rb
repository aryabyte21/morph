class Morph < Formula
  desc "Type-aware codemod CLI for AI agents and humans"
  homepage "https://github.com/aryabyte21/morph"
  version "0.1.0"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-darwin-arm64.tar.gz"
      sha256 "REPLACE_WITH_SHA256_FROM_RELEASE"
    else
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-darwin-amd64.tar.gz"
      sha256 "REPLACE_WITH_SHA256_FROM_RELEASE"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-arm64.tar.gz"
      sha256 "REPLACE_WITH_SHA256_FROM_RELEASE"
    else
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-amd64.tar.gz"
      sha256 "REPLACE_WITH_SHA256_FROM_RELEASE"
    end
  end

  def install
    bin.install "morph"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/morph --version")
  end
end
