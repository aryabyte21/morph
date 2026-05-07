class Morph < Formula
  desc "Type-aware codemod CLI for AI agents and humans"
  homepage "https://github.com/aryabyte21/morph"
  version "0.1.2"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-darwin-arm64.tar.gz"
      sha256 "c05686eee740b3786d08e1904a9149fe9d414faeb92feaa7d4a0b9470ddbdc92"
    else
      odie "morph: no prebuilt darwin-amd64 binary; install from source via opam"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-arm64.tar.gz"
      sha256 "877f4fac02fe640177c25993a3e27fb45d278b0b2806c13aec247bcbb35ac641"
    else
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-amd64.tar.gz"
      sha256 "d5a63fa1851f8238b55cbcefeaf3a5e16fb38fa1bcdc195cad723f4dca126f90"
    end
  end

  def install
    bin.install "morph"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/morph --version")
  end
end
