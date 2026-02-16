import XCTest
import SwiftTreeSitter
import TreeSitterLune

final class TreeSitterLuneTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_lune())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Lune grammar")
    }
}
