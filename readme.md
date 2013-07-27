##CrystalAtf

CrystalAtf - это оптимизированный atf и dds конвертер, базирующийся на библиотеке CrystalDXT.

##Использование

Поддерживаемые выходные форматы сжатия DXT1,DXT5,RGB888,RGBA8888
Поддерживаемые входные форматы: '.psd' (только в режиме совместимости и без прозрачности) ,'.png','.bmp','.jpg','.jpe','.jpeg',. 

Пример простой конвертации

CrystalAtf.exe somepicture.png

Указание выходного файла 

CrystalAtf.exe somepicture.png->atf/picture.atf

Конвертация по маске (? - один любой символ, * - 0 и более любых символов)

CrystalAtf.exe images/src/?image*.*

Указание выходного сжатия (по умолчанию выбирается между dxt1 и dxt5, в зависимости от наличия прозрачных пикселей)

CrystalAtf.exe -rgba somepicture.png

Полный список флагов сжатия
-auto - DXT1 или DXT5 (зависит от наличия прозрачных пикселей)
-dxt1 - DXT1
-dxt5 - DXT5
-rgba - RGB888 или RGBA8888 (зависит от наличия прозрачных пикселей)

Указать dds как выходной формат

CrystalAtf.exe -dds somepicture.png

Указать atf как выходной формат (установлен по умолчанию)

CrystalAtf.exe -atf somepicture.png

Отключить генерацию мип уровней

CrystalAtf.exe -nomip somepicture.png

Включить генерацию мип уровней (установлен по умолчанию)

CrystalAtf.exe -mip somepicture.png

CrystalAtf позволяет обрабатывать изображения в пакетном режиме и комбинировать различные флаги

CrystalAtf.exe -dds -nomip picture1.jpeg -atf -rgb pictures/*.* -mip -dxt1 pic999.psd
