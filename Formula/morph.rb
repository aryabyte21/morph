class Morph < Formula
  desc "Type-aware codemod CLI for AI agents and humans"
  homepage "https://github.com/aryabyte21/morph"
  version "0.2.0"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-darwin-arm64.tar.gz"
      sha256 "36dcca6d19a8876b5398b1f10b1b0e9098dabede317fb34e59f5bae28a04f72d"
    else
      odie "morph: no prebuilt darwin-amd64 binary; install from source via opam"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-arm64.tar.gz"
      sha256 "f6c7e74cf5d6c60b16a1ed89ad8992628474c0067a58a8d76d58cd0b84218d22"
    else
      url "https://github.com/aryabyte21/morph/releases/download/v#{version}/morph-linux-amd64.tar.gz"
      sha256 "cce9d1e503601a6a3830d28a68be569579185d916cda403f27ef0eefa53748f1"
    end
  end

  def install
    bin.install "morph"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/morph --version")
  end
end
