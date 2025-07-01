use crate::{SpirvTargetEnv, SpirvTargetParseError};
use serde::ser::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

impl Serialize for SpirvTargetEnv {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.target_triple())
    }
}

impl<'de> Deserialize<'de> for SpirvTargetEnv {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let target = String::deserialize(deserializer)?;
        Self::parse_triple(&target).map_err(|_e| serde::de::Error::unknown_variant(&target, &[]))
    }
}

pub fn serialize_target<S>(
    target: &Option<Result<SpirvTargetEnv, SpirvTargetParseError>>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    // cannot use `transpose()` due to target being a ref, not a value
    let option = match target {
        None => None,
        Some(Ok(e)) => Some(*e),
        Some(Err(_e)) => Err(Error::custom(
            "cannot serialize `target` that failed to parse",
        ))?,
    };
    Serialize::serialize(&option, serializer)
}

pub fn deserialize_target<'de, D>(
    deserializer: D,
) -> Result<Option<Result<SpirvTargetEnv, SpirvTargetParseError>>, D::Error>
where
    D: Deserializer<'de>,
{
    Ok(Ok(<Option<SpirvTargetEnv> as Deserialize>::deserialize(
        deserializer,
    )?)
    .transpose())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpirvTargetEnv, SpirvTargetParseError};

    #[test]
    fn test_serde_roundtrip() {
        for env in SpirvTargetEnv::iter() {
            let json = serde_json::to_string(&env).unwrap();
            let deserialize: SpirvTargetEnv = serde_json::from_str(&json).unwrap();
            assert_eq!(env, deserialize);
        }
    }

    #[test]
    fn test_serde_target_roundtrip() {
        #[derive(Clone, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
        struct FakeSpirvBuilder {
            #[serde(
                serialize_with = "serialize_target",
                deserialize_with = "deserialize_target"
            )]
            target: Option<Result<SpirvTargetEnv, SpirvTargetParseError>>,
        }

        for env in SpirvTargetEnv::iter() {
            let builder = FakeSpirvBuilder {
                target: Some(Ok(env)),
            };
            let json = serde_json::to_string(&builder).unwrap();
            let deserialize: FakeSpirvBuilder = serde_json::from_str(&json).unwrap();
            assert_eq!(builder, deserialize);
        }
    }
}
