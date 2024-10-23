use clap::Parser;
use anyhow::Result;
use reqwest;
use tokio;
use serde_json;

mod crossref;

#[derive(Parser)]
struct Cli {
    doi: String,
    isbn: String,
}

#[tokio::main]
async fn main() -> Result<()>{
    let args = Cli::parse();
    crossref(&args.doi).await?;
    
    // let content = std::fs::read_to_string(&args.path)
    //     .with_context(|| format!("Couldn't read the file!, {}", args.path.display()))?;

    // for line in content.lines() {
    // 	if line.contains(&args.pattern){
    // 	    println!("{}", line);
    // 	}
    // }

    Ok(())
}


async fn crossref(doi: &String) -> Result<()> {
    let url = format!("https://api.crossref.org/works/{}", doi);
    let body = reqwest::get(url)
        .await?
	.text()
	.await?;
    let response: crossref::ApiResponse = serde_json::from_str(&body)?;
    println!("{response:?}");
    Ok(())
}
