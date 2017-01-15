# arsenic
parser/compiler for knitting patterns

```
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----||||||||||O|O||/----|/O||/O|--\O\OO/O/--|O\O||||--/||O||O\|/O||O||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----|||||||||O|O|||/----|/O||/O|--|\O|/O/|--||||O\O|--/|||O\O|||O/O|||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----\|O|O|||O|O||||/----|/O||/O|--||O/\O||--|O\O||||--/O||\O|O\O|O/||O\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----\||O|O||||||||||----|/O||/O|--\O\OO/O/--||||O\O|--/|O|||O\|/O|||O|\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----\|||O|O|||||||||----|/O||/O|--|\O|/O/|--|O\O||||--/||O||O\|/O||O||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
---------------------|\O\O|--|||O|||\|/|||O|||----\||||O|O|||O|O|/----|/O||/O|--||O/\O||--||||O\O|--/|||O\O|||O/O|||\----
---------------------|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----||||||||||O|O||/----|/O||/O|--\O\OO/O/--|O\O||||--/O||\O|O\O|O/||O\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----|||||||||O|O|||/----|/O||/O|--|\O|/O/|--||||O\O|--/|O|||O\|/O|||O|\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----\|O|O|||O|O||||/----|/O||/O|--||O/\O||--|O\O||||--/||O||O\|/O||O||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----\||O|O||||||||||----|/O||/O|--\O\OO/O/--||||O\O|--/|||O\O|||O/O|||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----\|||O|O|||||||||----|/O||/O|--|\O|/O/|--|O\O||||--/O||\O|O\O|O/||O\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
---------------------|\O\O|--|||/|||O|O|||\|||----\||||O|O|||O|O|/----|/O||/O|--||O/\O||--||||O\O|--/|O|||O\|/O|||O|\----
---------------------|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----||||||||||O|O||/----|/O||/O|--\O\OO/O/--|O\O||||--/||O||O\|/O||O||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----|||||||||O|O|||/----|/O||/O|--|\O|/O/|--||||O\O|--/|||O\O|||O/O|||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----\|O|O|||O|O||||/----|/O||/O|--||O/\O||--|O\O||||--/O||\O|O\O|O/||O\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----\||O|O||||||||||----|/O||/O|--\O\OO/O/--||||O\O|--/|O|||O\|/O|||O|\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----\|||O|O|||||||||----|/O||/O|--|\O|/O/|--|O\O||||--/||O||O\|/O||O||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
---------------------|\O\O|--|||O|||\|/|||O|||----\||||O|O|||O|O|/----|/O||/O|--||O/\O||--||||O\O|--/|||O\O|||O/O|||\----
---------------------|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----||||||||||O|O||/----|/O||/O|--\O\OO/O/--|O\O||||--/O||\O|O\O|O/||O\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----|||||||||O|O|||/----|/O||/O|--|\O|/O/|--||||O\O|--/|O|||O\|/O|||O|\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||/|||O|O|||\|||----\|O|O|||O|O||||/----|/O||/O|--||O/\O||--|O\O||||--/||O||O\|/O||O||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---|||-||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----\||O|O||||||||||----|/O||/O|--\O\OO/O/--||||O\O|--/|||O\O|||O/O|||\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|O|||/O\O\|||O|--|\O\O|--|||O|||\|/|||O|||----\|||O|O|||||||||----|/O||/O|--|\O|/O/|--|O\O||||--/O||\O|O\O|O/||O\----
----|||||||||||||||--|O/O/|--|||||||||||||||||--||||||||||||||||||||---O/--O/---||||||||--||||||||--|||||||||||||||||----
----|||||||||||||||--||||||--|||||||||||||||||--||||||||||||||||||||--||||||||--||||||||--||||||||--|||||||||||||||||----
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
----|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||----
----|O\|||O\||||/O||||O\O||O\|||O\|||O\O|||/O|||||/O||O\O|||||/O||O\|||O\|||O\|||O\|||O\|||||O\O||O\||||/O|||/O||O\O|----
----|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||----
----|O\O|||/O||O\O||||||||||/O||||||||/O||O\O||||O\||||/O||||O\O|||||||O\|||||||||/O||O\O||||||||||/O||O\O||O\|||O\||----
----|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||----
----|O\|||O\||||/O|||||||||O\|||O\O||O\|||O\||||||||||O\||||||/O|||||||O\|||O\||||||||O\|||||O\|||O\|||O\||||||||||||----
----|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||----
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
```