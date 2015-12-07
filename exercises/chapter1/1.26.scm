

;;; Answer:
;;; Recursive programming in this problem is about breaking down calculations into
;;; smaller problems and NOT calculating a subproblem more than once.
;;; Louis Reasoner creates many duplicate branches of his expmod procedure here.
;;; In fact, he spawns 2 identical expmod calculations for every successful halving, bringing
;;; the order of the algorithm right back up to O(n).
