erl-finger-tree
===============

[FingerTree](http://www.soi.city.ac.uk/~ross/papers/FingerTree.html)のErlangでの実装例

* 論文の内容を比較的忠実に実装
  * lazinessが必要な箇所は、適宜strictな方法で置き換え
  * 処理速度を向上させるための最適化はほとんど行われていない
* FingerTreeの特徴メモ
  * いろいろな性質の列を実装するための汎用的なデータ構造
  * 列の両端への挿入・削除が、償却的な定数オーダーで行える
  * 列同士の結合は対数オーダー
  * また、ツリーの各ノードに measurement と呼ばれる汎用的な付加フィールドを持たせていることが特徴
      * measurement に応じたツリーの分割が対数オーダーで行える
      * measurement の算出方法をカスタマイズすることで、例えば以下のようなデータ構造が簡単かつ効率的に実装可能
          * ランダムアクセス可能な配列 (実装例は ft_que.erl を参照)
          * 優先順位付きキュー (実装例は ft_prioque.erl を参照)
          * ソートされたシーケンス (実装例は ft_ordseq.erl を参照)
          * Interval Tree (この部分は飛ばしたので詳しいことは不明)
