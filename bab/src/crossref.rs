use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct ApiResponse {
    status: String,
    #[serde(rename = "message-type")]
    message_type: String,
    #[serde(rename = "message-version")]
    message_version: String,
//    message: Message,
}

// #[derive(Serialize, Deserialize, Debug)]
// struct Message {
//     indexed: Indexed,
//     #[serde(rename = "update-to")]
//   //  update_to: Vec<UpdateTo>,
//   //  #[serde(rename = "reference-count")]
//     reference_count: u32,
//     publisher: String,
//     issue: String,
//   //  license: Vec<License>,
//     #[serde(rename = "content-domain")]
//  //   content_domain: ContentDomain,
//  //   #[serde(rename = "short-container-title")]
//     short_container_title: Vec<String>,
//     DOI: String,
//     #[serde(rename = "type")]
//     type_: String,
//     created: Timestamped,
//     #[serde(rename = "update-policy")]
//     update_policy: String,
//     source: String,
//     #[serde(rename = "is-referenced-by-count")]
//     is_referenced_by_count: u32,
//     title: Vec<String>,
//     prefix: String,
//     volume: String,
//     author: Vec<Author>,
//     member: String,
//     #[serde(rename = "published-online")]
//     published_online: DateParts,
//     #[serde(rename = "container-title")]
//     container_title: Vec<String>,
//     language: String,
//   //  link: Vec<Link>,
//     deposited: Timestamped,
//     score: f64,
//   //  resource: Resource,
//   //  issued: DateParts,
//     #[serde(rename = "references-count")]
//     references_count: u32,
//     #[serde(rename = "journal-issue")]
//  //   journal_issue: JournalIssue,
//     #[serde(rename = "alternative-id")]
//     alternative_id: Vec<String>,
//     URL: String,
//     ISSN: Vec<String>,
//     #[serde(rename = "issn-type")]
//     issn_type: Vec<IssnType>,
//     subject: Vec<String>,
//     #[serde(rename = "article-number")]
//     article_number: String,
// //    assertion: Vec<Assertion>,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Indexed {
//     #[serde(rename = "date-parts")]
//     date_parts: Vec<Vec<u32>>,
//     #[serde(rename = "date-time")]
//     date_time: String,
//     timestamp: u64,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct UpdateTo {
//     updated: Timestamped,
//     DOI: String,
//     #[serde(rename = "type")]
//     type_: String,
//     label: String,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct License {
//     start: Timestamped,
//     #[serde(rename = "content-version")]
//     content_version: String,
//     #[serde(rename = "delay-in-days")]
//     delay_in_days: u32,
//     URL: String,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct ContentDomain {
//     domain: Vec<String>,
//     #[serde(rename = "crossmark-restriction")]
//     crossmark_restriction: bool,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Timestamped {
//     #[serde(rename = "date-parts")]
//     date_parts: Vec<Vec<u32>>,
//     #[serde(rename = "date-time")]
//     date_time: String,
//     timestamp: u64,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Author {
//     #[serde(rename = "ORCID")]
//     orcid: Option<String>,
//     #[serde(rename = "authenticated-orcid")]
// //    authenticated_orcid: bool,
//     given: String,
//     family: String,
//     sequence: String,
//     affiliation: Vec<String>,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Link {
//     URL: String,
//     #[serde(rename = "content-type")]
//     content_type: String,
//     #[serde(rename = "content-version")]
//     content_version: String,
//     #[serde(rename = "intended-application")]
//     intended_application: String,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Resource {
//     primary: Primary,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Primary {
//     URL: String,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct JournalIssue {
//     issue: String,
//     #[serde(rename = "published-online")]
//     published_online: DateParts,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct DateParts {
//     #[serde(rename = "date-parts")]
//     date_parts: Vec<Vec<u32>>,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct IssnType {
//     #[serde(rename = "type")]
//     type_: String,
//     value: String,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct Assertion {
//     value: String,
//     order: u32,
//     name: String,
//     label: String,
//     group: AssertionGroup,
// }

// #[derive(Serialize, Deserialize, Debug)]
// struct AssertionGroup {
//     name: String,
//     label: String,
// }
