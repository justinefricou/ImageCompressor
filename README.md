# ImageCompressor

From a file listing all the pixels of an image with their position and color, this program regroups the pixels into a given number of clusters, according to their colors.

USAGE: ./imageCompressor  -n N  -l L  -f F
    N  number of colors in the final image
    L  convergence limit
    F  path to the file containing the pixels

Example of an input file:
$ cat input_file
(0,0) (66,20,26)
(0,1) (98,99,233)
(0,2) (45,12,167)
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (20,27,67)
(2,0) (88,77,211)
(2,1) (1,56,37)
(2,2) (15,89,40)

And a possible output:
$ ./imageCompressor -n 2 -l 0.8 -f input_file
--
(77,63,204)
-
(0,1) (98,99,233)
(2,0) (88,77,211)
(0,2) (45,12,167)
--
(35,36,45)
-
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (20,27,67)
(2,1) (1,56,37)
(0,0) (66,20,26)
(2,2) (15,89,40)
