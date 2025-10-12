pub struct RomHeader {
    pub copyright: [u8; 8], // TMR SEGA
    pub checksum: u16,
    pub product_code: u32,
    pub version: u8,
    pub console: Option<Console>,
    pub region: Option<Region>,
    pub declared_size: u8, // do not decode, we won't use this for now
}

pub enum Console {
    #[allow(clippy::upper_case_acronyms)]
    SMS,
    GG,
}

pub enum Region {
    Japan,
    Export,
    International,
}

const HEADER_OFFSET: usize = 0x7ff0;
const HEADER_LEN: usize = 16;

pub fn decoder_rom_header(rom: &[u8]) -> Result<RomHeader, String> {
    if rom.len() < HEADER_OFFSET + HEADER_LEN {
        return Err("rom too short to have header at 0x7ff0".into());
    }
    let raw_header = &rom[HEADER_OFFSET..(HEADER_OFFSET + HEADER_LEN)];
    let (console, region) = match raw_header[15] >> 4 {
        3 => (Some(Console::SMS), Some(Region::Japan)),
        4 => (Some(Console::SMS), Some(Region::Export)),
        5 => (Some(Console::GG), Some(Region::Japan)),
        6 => (Some(Console::GG), Some(Region::Export)),
        7 => (Some(Console::GG), Some(Region::International)),
        _ => (None, None),
    };
    let header = RomHeader {
        copyright: raw_header[..8]
            .try_into()
            .expect("shouldn't fail, size was checked before"),
        checksum: raw_header[10] as u16 | ((raw_header[11] as u16) << 8),
        product_code: raw_header[12] as u32
            | ((raw_header[13] as u32) << 8)
            | ((raw_header[14] as u32 & 0xf0) << 12),
        version: raw_header[14] & 0xf,
        console,
        region,
        declared_size: raw_header[15] & 0xf,
    };
    Ok(header)
}
