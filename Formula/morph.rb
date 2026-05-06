class Morph < Formula
  desc "Type-aware codemod CLI for AI agents and humans"
  homepage "https://github.com/aryabyte21/morph"
  version "0.1.0"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-darwin-arm64.tar.gz"
      sha256 "1f6eafe3337d81f89a17c4b428ffe077050dbfd1737fcc65991232afbecc3b13"
    else
      odie "morph: no prebuilt darwin-amd64 binary in v0.1.0; install from source via opam"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-arm64.tar.gz"
      sha256 "d9e58d211bc3b9c6075bbe886faef2fc3aeb9d641991553cf34dab90ccdac8fe"
    else
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-amd64.tar.gz"
      sha256 "8069d627c225449d91dd9ba71c0b43fb1ca40fb903bfa0d6b60c460e11af0fd8"
    end
  end

  def install
    bin.install "morph"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/morph --version")
  end
end
