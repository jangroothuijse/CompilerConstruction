definition module SSMWriter

import SSMCodeGen

class writeSSM a :: *File a -> *File
instance writeSSM SSMCode
