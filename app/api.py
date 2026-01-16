from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from pydantic import BaseModel
from app.translator import translate_cobol_to_java
import logging
import os
from pathlib import Path
from dotenv import load_dotenv

# Load environment variables from .env file
env_path = Path('.') / '.env'
if env_path.exists():
    load_dotenv(dotenv_path=env_path)
    print(f"✓ Loaded environment variables from {env_path}")
else:
    print(f"⚠️ .env file not found at {env_path}")
    print("  Looking for .env in parent directories...")
    # Try to load from project root
    load_dotenv()
    print("  Loaded environment variables")

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Create FastAPI app
app = FastAPI(
    title="AI COBOL to Java Modernization Engine",
    description="Proof of Concept: Enterprise banking legacy system modernization",
    version="1.0.0"
)

# Get the absolute path to the app directory
app_dir = Path(__file__).parent

# Setup static files and templates
static_dir = app_dir / "static"
templates_dir = app_dir / "templates"

print(f"App directory: {app_dir}")
print(f"Static directory: {static_dir} (exists: {static_dir.exists()})")
print(f"Templates directory: {templates_dir} (exists: {templates_dir.exists()})")

# Create directories if they don't exist
static_dir.mkdir(exist_ok=True)
templates_dir.mkdir(exist_ok=True)

# Mount static files and setup templates
app.mount("/static", StaticFiles(directory=str(static_dir)), name="static")
templates = Jinja2Templates(directory=str(templates_dir))

class TranslationRequest(BaseModel):
    cobol_code: str
    model: str = "llama-3.1-8b-instant"

class TranslationResponse(BaseModel):
    java_code: str
    explanation: str

@app.get("/", response_class=HTMLResponse)
async def serve_ui(request: Request):
    """Serve the main web interface"""
    try:
        return templates.TemplateResponse("index.html", {"request": request})
    except Exception as e:
        logger.error(f"Error serving UI: {e}")
        return HTMLResponse(f"""
        <html>
            <head><title>Error</title></head>
            <body>
                <h1>Error Loading Interface</h1>
                <p>index.html not found in templates directory.</p>
                <p>Error: {str(e)}</p>
                <p>Templates directory: {templates_dir}</p>
            </body>
        </html>
        """)

@app.post("/translate", response_model=TranslationResponse)
async def translate_cobol(request: TranslationRequest):
    try:
        logger.info("Received translation request")
        
        if not request.cobol_code or request.cobol_code.strip() == "":
            raise HTTPException(
                status_code=400,
                detail="COBOL source code cannot be empty"
            )
        
        result = translate_cobol_to_java(request.cobol_code, request.model)
        
        return TranslationResponse(
            java_code=result["java_code"],
            explanation=result["explanation"]
        )
        
    except Exception as e:
        logger.error(f"Translation failed: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"Translation failed: {str(e)}"
        )

@app.get("/sample/{sample_name}")
async def get_sample_code(sample_name: str):
    """Get sample COBOL code"""
    sample_dir = Path("samples")
    valid_samples = {
        "loan_interest": "sample_loan_interest.cob",
        "account_validation": "sample_account_validation.cob",
        "transaction_ledger": "sample_transaction_ledger.cob"
    }
    
    if sample_name not in valid_samples:
        raise HTTPException(status_code=404, detail="Sample not found")
    
    file_path = sample_dir / valid_samples[sample_name]
    
    if not file_path.exists():
        raise HTTPException(status_code=404, detail="Sample file not found")
    
    try:
        content = file_path.read_text(encoding='utf-8')
        return {"filename": valid_samples[sample_name], "content": content}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error reading sample: {str(e)}")

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy", 
        "service": "cobol-java-translator",
        "api_key_set": bool(os.getenv("GROQ_API_KEY"))
    }

@app.get("/routes")
async def list_routes():
    """List all available routes"""
    routes = []
    for route in app.routes:
        routes.append({
            "path": route.path,
            "name": route.name if hasattr(route, "name") else "unnamed",
            "methods": list(route.methods) if hasattr(route, "methods") else []
        })
    return routes

if __name__ == "__main__":
    import uvicorn
    print("=" * 60)
    print("Starting AI COBOL Modernization Engine")
    print("=" * 60)
    
    # Check GROQ_API_KEY
    api_key = os.getenv("GROQ_API_KEY")
    if api_key:
        print(f"✓ GROQ_API_KEY is set (length: {len(api_key)})")
    else:
        print("⚠️  GROQ_API_KEY is not set!")
        
    print(f"Available routes:")
    for route in app.routes:
        print(f"  {route.path}")
    
    print("=" * 60)
    uvicorn.run(app, host="127.0.0.1", port=8000)