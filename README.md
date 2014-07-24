LearningSICP
============

- 評価方法
	- 正規順序評価 : 完全に展開して簡約する
	- 適用順序評価 : 引数を評価してから適用


- [TODO]
	- 1.25 figure out why outliner exist	 

- Thought
	- Check iterative / recursive
		- Traverse all function call (make a call graph, may contain cycle, mark if tail call)
		- Check if all cycle in graph are tail-call-cycle
			- Check if all edge on cycle is tail call
