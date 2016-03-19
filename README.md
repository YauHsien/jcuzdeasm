## Java Class Deassembler

To deassemble and recontruct Java code from classes in Jar.

``jcuzdeasm`` is pronunciated as "Java class deassem(blying)".

### Usage

Put ``.jar`` files into the folder ``priv/`` and use following command:

* ``make xjar jar=<jar file>``: to extract files from a ``.jar`` file; files will be put in the temporary folder ``/tmp``.
* ``make tjar jar=<jar file>``: to list files in a ``.jar`` file.
* ``make cls2asm jar=<jar file>``: to deassembly ``.class``; files are saved as ``.javasm``.
* ``make build``: to build Erlang application ``jczdzm``.
* ``make asm2code jar=<jar file>`` to convert ``.javasm`` to ``.java``.
