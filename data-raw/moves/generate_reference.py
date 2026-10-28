"""Generate a bounded MOVES reference export using a private MariaDB instance.

Production helper only: the resulting CSV files are all that R users need.
Runs one Java process/local worker with a 512 MiB heap and small DB buffers.
Always shuts down this private server on completion/failure.
"""
import csv
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
import zipfile

HERE = Path(__file__).resolve().parent
ROOT = Path(os.environ.get('MOVES_WORK_DIR', str(HERE.parents[2] / 'moves_work'))).expanduser().resolve()
MODEL = ROOT / 'model'
MARIA = ROOT / 'tools/mariadb-10.11.14-linux-systemd-x86_64'
DATA = ROOT / 'mariadb-data'
PORT = 13316
SOCKET = '/tmp/opencode/vein-moves.sock'
CLIENT = [str(MARIA / 'bin/mariadb'), '--no-defaults', '--socket=' + SOCKET,
          '--user=root', '--batch', '--raw', '--local-infile=1']
ENV = dict(os.environ, GOMAXPROCS='1', GOMEMLIMIT='256MiB',
           JAVA_TOOL_OPTIONS='-XX:ActiveProcessorCount=1 -Djava.awt.headless=true')


def sql(query):
    return subprocess.check_output(CLIENT + ['-e', query], universal_newlines=True)


def export(query, filename):
    text = sql(query)
    with (ROOT / 'exports' / filename).open('w', newline='') as target:
        writer = csv.writer(target)
        for row in csv.reader(io.StringIO(text), delimiter='\t', quoting=csv.QUOTE_NONE):
            writer.writerow(['' if value == 'NULL' else value for value in row])


def main():
    shutil.copy2(str(HERE / 'reference.mrs'), str(ROOT / 'reference.mrs'))
    with socket.socket() as probe:
        probe.bind(('127.0.0.1', PORT))
    if not (DATA / 'mysql').exists():
        subprocess.run([str(MARIA / 'scripts/mariadb-install-db'), '--no-defaults',
                        '--basedir=' + str(MARIA), '--datadir=' + str(DATA),
                        '--auth-root-authentication-method=normal', '--skip-test-db'], check=True)
    config = ROOT / 'my.cnf'
    config.write_text('''[mysqld]
basedir={maria}
datadir={data}
socket={sock}
port={port}
bind-address=127.0.0.1
pid-file={root}/mariadb.pid
log-error={root}/mariadb.log
default-storage-engine=MyISAM
secure-file-priv=
sql_mode=STRICT_TRANS_TABLES,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION
lower_case_table_names=1
character-set-server=utf8mb4
collation-server=utf8mb4_unicode_ci
init-connect='SET NAMES utf8mb4'
key_buffer_size=64M
innodb_buffer_pool_size=64M
myisam_sort_buffer_size=32M
sort_buffer_size=1M
read_buffer_size=1M
join_buffer_size=1M
max_connections=128
table_open_cache=512
tmp_table_size=32M
max_heap_table_size=32M
local_infile=1
'''.format(maria=MARIA, data=DATA, sock=SOCKET, port=PORT, root=ROOT))
    server = subprocess.Popen([str(MARIA / 'bin/mariadbd'), '--defaults-file=' + str(config)])
    try:
        for attempt in range(60):
            if server.poll() is not None:
                raise RuntimeError('Private database exited; see mariadb.log')
            try:
                subprocess.check_output(CLIENT + ['-e', 'SELECT 1'], stderr=subprocess.DEVNULL)
                break
            except subprocess.CalledProcessError:
                time.sleep(1)
        else:
            raise RuntimeError('Private database did not become ready')
        if not (ROOT / 'database-imported').exists():
            print('Importing the bundled default database (streaming)...', flush=True)
            with zipfile.ZipFile(str(MODEL / 'database/Setup/movesdb20260729.zip')) as archive:
                with archive.open('movesdb20260729.sql') as source:
                    process = subprocess.Popen(CLIENT, stdin=subprocess.PIPE)
                    shutil.copyfileobj(source, process.stdin, 1024 * 1024)
                    process.stdin.close()
                    if process.wait() != 0:
                        raise RuntimeError('Default database import failed')
            (ROOT / 'database-imported').touch()
        sql("CREATE USER IF NOT EXISTS 'moves'@'localhost' IDENTIFIED BY 'moves'; "
            "CREATE USER IF NOT EXISTS 'moves'@'127.0.0.1' IDENTIFIED BY 'moves'; "
            "GRANT ALL ON *.* TO 'moves'@'localhost'; "
            "GRANT ALL ON *.* TO 'moves'@'127.0.0.1';")
        (MODEL / 'MySQL.txt').write_text(str(PORT))
        for filename, old, new in (
                ('MOVESConfiguration.txt', 'generators/externalgenerator64.exe', 'generators/externalgenerator'),
                ('WorkerConfiguration.txt', 'calc/externalcalculatorgo64.exe', 'calc/externalcalculatorgo')):
            path = MODEL / filename
            path.write_text(path.read_text().replace(old, new))
        sql('CREATE DATABASE IF NOT EXISTS vein_reference_input; '
            'CREATE TABLE IF NOT EXISTS vein_reference_input.zonemonthhour '
            'LIKE movesdb20260729.zonemonthhour; '
            'TRUNCATE vein_reference_input.zonemonthhour; '
            'INSERT INTO vein_reference_input.zonemonthhour '
            'SELECT z.* FROM movesdb20260729.zonemonthhour z '
            'JOIN movesdb20260729.zone s USING (zoneID) WHERE s.countyID=26161; '
            'UPDATE vein_reference_input.zonemonthhour SET relHumidity=50, '
            'temperature=CASE hourID WHEN 1 THEN 68 WHEN 2 THEN 50 WHEN 3 THEN 86 ELSE 68 END;')
        print('Input grid:', sql('SELECT * FROM vein_reference_input.zonemonthhour '
                                'WHERE monthID=1 AND hourID<=3'), flush=True)
        paths = ['.'] + [node.attrib['location'] for node in
                         ET.parse(str(MODEL / 'build.xml')).find("path[@id='classpath']")]
        cp = os.pathsep.join(paths)
        mode = sys.argv[1] if len(sys.argv) > 1 else 'reference'
        if mode == 'reference':
            runspec, output = 'reference.mrs', 'vein_reference_output'
        elif mode == 'events':
            runspec, output = 'events.mrs', 'vein_events_output'
        else:
            raise ValueError('Mode must be reference or events')
        shutil.copy2(str(HERE / runspec), str(ROOT / runspec))
        print('Executing MOVES6 ' + mode + ' RunSpec...', flush=True)
        subprocess.run(['java', '-Xmx512m', '-cp', cp,
                        'gov.epa.otaq.moves.master.commandline.MOVESCommandLine',
                        '-r', str(ROOT / runspec)], cwd=str(MODEL), env=ENV,
                       check=True, timeout=3600)
        run_id = int(sql('SELECT MAX(MOVESRunID) FROM ' + output + '.MOVESRun').splitlines()[-1])
        where = ' WHERE MOVESRunID=' + str(run_id)
        errors = sql('SELECT * FROM ' + output + '.MOVESError' + where)
        print(errors, flush=True)
        if len(errors.splitlines()) > 1:
            raise RuntimeError('MOVES reported errors for the current run')
        (ROOT / 'exports').mkdir(exist_ok=True)
        if mode == 'reference':
            count = sql('SELECT COUNT(*) FROM ' + output + '.rateperdistance' + where).splitlines()[-1]
            if int(count) == 0:
                raise RuntimeError('MOVES produced no distance rates; see model/moveslog.txt')
            export('SELECT * FROM ' + output + '.rateperdistance' + where +
                   ' AND processID=1 AND roadTypeID=5 AND modelYearID BETWEEN 2000 AND 2025 '
                   'ORDER BY MOVESRunID,pollutantID,modelYearID,hourID,avgSpeedBinID',
                   'rateperdistance.csv')
            export('SELECT * FROM movesdb20260729.avgspeedbin ORDER BY avgSpeedBinID',
                   'avgspeedbin.csv')
        else:
            for table, query in (
                    ('rateperstart',
                     'SELECT * FROM ' + output + '.rateperstart' + where +
                     ' AND processID=2 AND modelYearID BETWEEN 2000 AND 2025 '
                     'ORDER BY MOVESRunID,pollutantID,modelYearID,hourID,sourceTypeID,fuelTypeID'),
                    ('ratepervehicle',
                     'SELECT * FROM ' + output + '.ratepervehicle' + where +
                     ' AND processID IN (11,13) AND modelYearID BETWEEN 2000 AND 2025 '
                     'ORDER BY MOVESRunID,pollutantID,processID,modelYearID,hourID'),
                    ('rateperprofile',
                     'SELECT * FROM ' + output + '.rateperprofile' + where +
                     ' AND processID=12 AND modelYearID BETWEEN 2000 AND 2025 '
                     'ORDER BY MOVESRunID,pollutantID,modelYearID,hourID')):
                count = sql('SELECT COUNT(*) FROM ' + output + '.' + table + where).splitlines()[-1]
                if int(count) == 0:
                    raise RuntimeError('MOVES produced no ' + table + ' rates')
                export(query, table + '.csv')
        suffix = '' if mode == 'reference' else '_events'
        export('SELECT * FROM ' + output + '.MOVESRun' + where,
               'movesrun' + suffix + '.csv')
        export('SELECT * FROM ' + output + '.MOVESError' + where,
               'moveserror' + suffix + '.csv')
        export('SELECT * FROM vein_reference_input.zonemonthhour '
               'WHERE monthID=1 AND hourID<=3 ORDER BY zoneID,hourID', 'zonemonthhour.csv')
        export('SELECT DISTINCT ft.fuelTypeID,ft.fuelTypeDesc,sut.sourceTypeID,'
               'sut.sourceTypeName FROM movesdb20260729.fueltype ft,'
               'movesdb20260729.sourceusetype sut,movesdb20260729.fuelengtechassoc feta '
               'WHERE ft.fuelTypeID=feta.fuelTypeID AND sut.sourceTypeID=feta.sourceTypeID '
               'ORDER BY sut.sourceTypeID,ft.fuelTypeID', 'vehicles.csv')
        manifest = {}
        for path in (ROOT / 'exports').glob('*.csv'):
            manifest[path.name] = hashlib.sha256(path.read_bytes()).hexdigest()
        (ROOT / 'exports/SHA256.json').write_text(json.dumps(manifest, indent=2) + '\n')
        print(mode.capitalize() + ' export complete: ' + str(ROOT / 'exports'), flush=True)
    finally:
        subprocess.run([str(MARIA / 'bin/mariadb-admin'), '--no-defaults',
                        '--socket=' + SOCKET, '--user=root', 'shutdown'], timeout=30)
        server.wait(timeout=30)


if __name__ == '__main__':
    main()
