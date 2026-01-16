class CobolTranslatorUI {
    constructor() {
        this.cobolTextarea = document.getElementById('cobolCode');
        this.javaOutput = document.getElementById('javaCode');
        this.explanationDiv = document.getElementById('explanation');
        this.translateBtn = document.getElementById('translateBtn');
        this.clearBtn = document.getElementById('clearBtn');
        this.loadingDiv = document.querySelector('.loading');
        this.statusText = document.querySelector('.status-text');
        this.lineNumbers = document.querySelector('.line-count');
        
        this.setupEventListeners();
        this.updateLineNumbers();
        this.updateStatus('Ready to translate COBOL to Java');
    }

    setupEventListeners() {
        this.translateBtn.addEventListener('click', () => this.translateCode());
        this.clearBtn.addEventListener('click', () => this.clearAll());
        
        this.cobolTextarea.addEventListener('input', () => {
            this.updateLineNumbers();
            this.updateStatus('Modified - Click Translate to generate Java');
        });
        
        document.querySelectorAll('.sample-btn').forEach(btn => {
            btn.addEventListener('click', (e) => {
                const sampleName = e.currentTarget.dataset.sample;
                this.loadSample(sampleName);
            });
        });
    }

    updateLineNumbers() {
        const lines = this.cobolTextarea.value.split('\n').length;
        let numbers = '';
        for (let i = 1; i <= lines; i++) {
            numbers += i + '\n';
        }
        this.lineNumbers.textContent = numbers;
    }

    updateStatus(message) {
        this.statusText.textContent = message;
    }

    showLoading() {
        this.loadingDiv.style.display = 'block';
        this.translateBtn.disabled = true;
        this.translateBtn.innerHTML = '<i class="fas fa-spinner fa-spin"></i> Translating...';
    }

    hideLoading() {
        this.loadingDiv.style.display = 'none';
        this.translateBtn.disabled = false;
        this.translateBtn.innerHTML = '<i class="fas fa-code"></i> Translate to Java';
    }

    async loadSample(sampleName) {
        try {
            this.updateStatus(`Loading ${sampleName.replace('_', ' ')} sample...`);
            
            const response = await fetch(`/sample/${sampleName}`);
            if (!response.ok) throw new Error('Failed to load sample');
            
            const data = await response.json();
            this.cobolTextarea.value = data.content;
            this.updateLineNumbers();
            this.updateStatus(`Loaded sample: ${data.filename}`);
            
            this.javaOutput.textContent = '// Java code will appear here after translation\n// Click "Translate to Java" button';
            this.explanationDiv.textContent = '';
            
        } catch (error) {
            this.updateStatus(`Error loading sample: ${error.message}`);
            console.error('Sample loading error:', error);
        }
    }

    async translateCode() {
        const cobolCode = this.cobolTextarea.value.trim();
        
        if (!cobolCode) {
            this.updateStatus('Error: Please enter COBOL code');
            alert('Please enter COBOL code to translate');
            return;
        }

        this.showLoading();
        this.updateStatus('Sending COBOL code to AI translation engine...');

        try {
            const response = await fetch('/translate', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    cobol_code: cobolCode,
                    model: 'llama-3.1-8b-instant'
                })
            });

            if (!response.ok) {
                const error = await response.json();
                throw new Error(error.detail || 'Translation failed');
            }

            const data = await response.json();
            
            this.javaOutput.textContent = data.java_code;
            this.explanationDiv.textContent = data.explanation;
            
            this.updateStatus('Translation completed successfully');
            
            this.highlightJavaCode();
            
        } catch (error) {
            console.error('Translation error:', error);
            this.javaOutput.textContent = `// Translation Error\n// ${error.message}`;
            this.explanationDiv.textContent = 'An error occurred during translation. Please check your COBOL code and try again.';
            this.updateStatus(`Error: ${error.message}`);
        } finally {
            this.hideLoading();
        }
    }

    highlightJavaCode() {
        const code = this.javaOutput.textContent;
        let highlighted = code
            .replace(/(\/\/.*)/g, '<span class="comment">$1</span>')
            .replace(/(\/\*[\s\S]*?\*\/)/g, '<span class="comment">$1</span>')
            .replace(/\b(class|interface|enum|extends|implements|public|private|protected|static|final|void|return|new|if|else|for|while|do|switch|case|default|try|catch|finally|throw|throws)\b/g, '<span class="keyword">$1</span>')
            .replace(/\b(int|double|float|boolean|char|byte|short|long|String|BigDecimal|Integer|List|Map|Set|ArrayList|HashMap|HashSet)\b/g, '<span class="type">$1</span>')
            .replace(/\b(true|false|null)\b/g, '<span class="constant">$1</span>')
            .replace(/(@\w+)/g, '<span class="annotation">$1</span>');
        
        this.javaOutput.innerHTML = highlighted;
        
        const style = document.createElement('style');
        style.textContent = `
            .keyword { color: #0077cc; font-weight: bold; }
            .type { color: #aa00aa; font-weight: bold; }
            .comment { color: #008000; font-style: italic; }
            .constant { color: #aa5500; }
            .annotation { color: #808000; }
        `;
        document.head.appendChild(style);
    }

    clearAll() {
        this.cobolTextarea.value = '';
        this.javaOutput.textContent = '// Java code will appear here after translation\n// Click "Translate to Java" button';
        this.explanationDiv.textContent = '';
        this.updateLineNumbers();
        this.updateStatus('Cleared all content');
    }
}

document.addEventListener('DOMContentLoaded', () => {
    window.translatorUI = new CobolTranslatorUI();
});  