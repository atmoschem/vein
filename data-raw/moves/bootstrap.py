"""Prepare a private, single-threaded MOVES6 build; run with Python >= 3.6.

Downloads are streamed and SHA-256 verified. No system packages/services are
changed. This script prepares the software, not a running database server.
"""
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import tarfile
import urllib.request

HERE = Path(__file__).resolve().parent
ROOT = Path(os.environ.get('MOVES_WORK_DIR', str(HERE.parents[2] / 'moves_work'))).expanduser().resolve()
SOURCE = Path(os.environ.get('MOVES_SOURCE_DIR', str(HERE.parents[2] / 'moves'))).expanduser().resolve()
MODEL = ROOT / 'model'
TOOLS = ROOT / 'tools'
DOWNLOADS = ROOT / 'downloads'
for directory in (TOOLS, DOWNLOADS):
    directory.mkdir(parents=True, exist_ok=True)


def install(url, checksum, directory):
    archive = DOWNLOADS / url.rsplit('/', 1)[1]
    if not archive.exists():
        part = archive.with_suffix('.part')
        print('Downloading ' + url, flush=True)
        with urllib.request.urlopen(url, timeout=120) as src, part.open('wb') as dst:
            shutil.copyfileobj(src, dst, 1024 * 1024)
        part.rename(archive)
    digest = hashlib.sha256()
    with archive.open('rb') as src:
        for block in iter(lambda: src.read(1024 * 1024), b''):
            digest.update(block)
    if digest.hexdigest() != checksum:
        raise RuntimeError('SHA-256 mismatch: ' + str(archive))
    if not (TOOLS / directory).exists():
        print('Extracting ' + archive.name, flush=True)
        with tarfile.open(str(archive), 'r|gz') as src:
            for member in src:
                target = (TOOLS / member.name).resolve()
                if str(target).startswith(str(TOOLS.resolve()) + os.sep):
                    src.extract(member, str(TOOLS))
                else:
                    raise RuntimeError('Invalid archive path: ' + member.name)


install('https://archive.mariadb.org/mariadb-10.11.14/bintar-linux-systemd-x86_64/'
        'mariadb-10.11.14-linux-systemd-x86_64.tar.gz',
        'f8710a80f71aae6c95d452468ae7ec9764ce06346fd76a9d0f0e5df6a385febf',
        'mariadb-10.11.14-linux-systemd-x86_64')
install('https://go.dev/dl/go1.26.10.linux-amd64.tar.gz',
        'ccfe7ae822d2c2f41f4fe7c956199967a4dac22d0ee0d2b2bfdb440aab3b20c5',
        'go')

if not MODEL.exists():
    shutil.copytree(str(SOURCE), str(MODEL),
                    ignore=shutil.ignore_patterns('.git'))

env = dict(os.environ, GOMAXPROCS='1', GOFLAGS='-p=1', GOMEMLIMIT='256MiB',
           GOTOOLCHAIN='local', GOROOT=str(TOOLS / 'go'),
           GOCACHE=str(ROOT / 'go-cache'),
           JAVA_TOOL_OPTIONS='-XX:ActiveProcessorCount=1 -Djava.awt.headless=true')
subprocess.run(['java', '-Xmx512m', '-cp', 'ant/lib/ant-launcher.jar',
                'org.apache.tools.ant.launch.Launcher', 'compile'],
               cwd=str(MODEL), env=env, check=True)
for folder, source, binary in (
        ('calc', 'externalcalculatorgo.go', 'externalcalculatorgo'),
        ('generators', 'externalgenerator.go', 'externalgenerator')):
    args = [str(TOOLS / 'go/bin/go'), 'build', '-p=1']
    if folder == 'generators':
        args += ['-mod=vendor']
    subprocess.run(args + ['-o', binary, source], cwd=str(MODEL / folder),
                   env=env, check=True)
print('MOVES Java and Linux Go build complete.', flush=True)
