import Foundation
import EmacsSwiftModule

enum DataString {
    case String(String)
    case Data(Data)
}

extension DataString: EmacsConvertible {
    func convert(within env: Environment) throws -> EmacsValue {
        switch self {
        case .String(let string):
            try string.convert(within: env)
        case .Data(let data):
            try data.convert(within: env)
        }
    }
    
    static func convert(from: EmacsValue, within env: Environment) throws -> DataString {
        throw EmacsError.customError(message: "do not convert Value to DataString")
    }
}
