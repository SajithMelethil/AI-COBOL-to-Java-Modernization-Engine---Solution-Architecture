import uvicorn
import os
import sys

# Ensure the app directory is in the Python path
app_dir = os.path.dirname(os.path.abspath(__file__))
parent_dir = os.path.dirname(app_dir)
if parent_dir not in sys.path:
    sys.path.insert(0, parent_dir)

try:
    from app.api import app
    
    if __name__ == "__main__":
        print("=" * 60)
        print("AI COBOL to Java Modernization Engine")
        print("=" * 60)
        print(f"Python: {sys.version}")
        print(f"Working directory: {os.getcwd()}")
        print(f"App directory: {app_dir}")
        print("=" * 60)
        
        # Check environment
        groq_key = os.getenv("GROQ_API_KEY")
        if not groq_key:
            print("⚠️  WARNING: GROQ_API_KEY environment variable is not set!")
            print("   Translation will fail without a valid API key.")
            print("   Set it with: $env:GROQ_API_KEY='your_key_here'")
        else:
            print("✓ GROQ_API_KEY is set")
        
        # Check if static and templates directories exist
        static_dir = os.path.join(app_dir, "static")
        templates_dir = os.path.join(app_dir, "templates")
        
        if not os.path.exists(static_dir):
            print(f"⚠️  WARNING: Static directory not found: {static_dir}")
            print("   Creating static directory...")
            os.makedirs(static_dir, exist_ok=True)
        
        if not os.path.exists(templates_dir):
            print(f"⚠️  WARNING: Templates directory not found: {templates_dir}")
            print("   Creating templates directory...")
            os.makedirs(templates_dir, exist_ok=True)
        
        print("=" * 60)
        print("Starting server...")
        print("🌐 Access the web interface at: http://localhost:8000")
        print("🛑 Press CTRL+C to stop the server")
        print("=" * 60)
        
        uvicorn.run(
            app,
            host="127.0.0.1",
            port=8000,
            log_level="info",
            reload=False
        )
        
except ImportError as e:
    print(f"❌ Import Error: {e}")
    print("Current Python path:")
    for path in sys.path:
        print(f"  {path}")
    print("\nTrying alternative import...")
    
    # Try absolute import
    import importlib.util
    spec = importlib.util.spec_from_file_location("api", os.path.join(app_dir, "api.py"))
    api_module = importlib.util.module_from_spec(spec)
    sys.modules["api"] = api_module
    spec.loader.exec_module(api_module)
    app = api_module.app
    
    print("✓ Successfully loaded app via alternative method")
    
    # Run the app
    print("=" * 60)
    print("Starting server...")
    uvicorn.run(
        app,
        host="127.0.0.1",
        port=8000,
        log_level="info",
        reload=False
    )