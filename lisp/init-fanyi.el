(use-package pdd
  :straight (pdd
	     :type git
	     :host github
	     :repo "emacsmirror/pdd"))
(use-package gt
  :straight (gt
	     :type git
	     :host github
	     :repo "lorniu/gt.el")
  :config
  (setq gt-langs '(en zh))
  (setq gt-taker-text 'word)      ; 默认情况下，初始文本是光标下的单词。如果有文本选中，优先使用选中文本
  (setq gt-taker-pick 'paragraph) ; 默认情况下，会按照段落标准分割初始文本。如果不想使用多段翻译，将其设置为 nil
  (setq gt-taker-prompt nil)      ; 默认情况下，没有 prompt 步骤。如果需要，将其设置为 t 或 'buffer
  ;; (setq gt-default-translator
  ;;       (gt-translator
  ;;        :engines (gt-google-engine)
  ;;        :render (gt-overlay-render :type 'help-echo)))
  ;; ts-1: 使用 Bing 在 en 和 zh 间进行翻译，翻译的是光标附近的单词或选中的文本，结果将以 overlay 的方式显示在当前位置
  ;; ts-2: 使用 Google 在 en, fr 和 ru 间进行翻译，翻译的是光标附近的句子或选中的文本，结果将以 overlay 的方式显示在当前位置
  ;; ts-3: 使用 Google 翻译 buffer 中所有长度大于 6 的单词，将鼠标放到被翻译的单词上后，翻译结果将以 popup 方式显示
  (setq gt-preset-translators
	`((ts-bing-word . ,(gt-translator
			    :taker (gt-taker :langs '(en zh) :text 'word)
			    :engines (gt-bing-engine)
			    :render (gt-overlay-render)))
	  (ts-bing-sentence . ,(gt-translator
				:taker (gt-taker :langs '(en zh) :text 'sentence)
				:engines (gt-bing-engine)
				:render (gt-overlay-render)))
	  (ts-google-sentence . ,(gt-translator
				  :taker (gt-taker :langs '(en zh) :text 'sentence)
				  :engines (gt-google-engine)
				  :render (gt-overlay-render)))
	  (ts-google-buffer-len-gt-6 . ,(gt-translator
					 :taker (gt-taker :langs '(en zh) :text 'buffer
							  :pick 'word :pick-pred (lambda (w) (length> w 6)))
					 :engines (gt-google-engine)
					 :render (gt-overlay-render :type 'help-echo)))))
  )

(provide 'init-fanyi)
