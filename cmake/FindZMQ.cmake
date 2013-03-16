# - Try to find ZMQ
# Once done this will define
#
#  ZMQ_FOUND - system has ZMQ
#  ZMQ_INCLUDE_DIRS - the ZMQ include directory
#  ZMQ_LIBRARIES - Link these to use ZMQ
#  ZMQ_DEFINITIONS - Compiler switches required for using ZMQ
#
#  Copyright (c) 2011 Lee Hambley <lee.hambley@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  
#  1. Redistributions of source code must retain the copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#  3. The name of the author may not be used to endorse or promote products 
#     derived from this software without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
#  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
#  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
#  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
#  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
#  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

if (ZMQ_LIBRARIES AND ZMQ_INCLUDE_DIRS)
  # in cache already
  set(ZMQ_FOUND TRUE)
else (ZMQ_LIBRARIES AND ZMQ_INCLUDE_DIRS)

  find_path(ZMQ_INCLUDE_DIR
    NAMES
      zmq.h
    PATHS
      /usr/include
      /usr/local/include
      /opt/local/include
      /sw/include
  )

  find_library(ZMQ_LIBRARY
    NAMES
      zmq
    PATHS
      /usr/lib
      /usr/local/lib
      /opt/local/lib
      /sw/lib
  )

  set(ZMQ_INCLUDE_DIRS
    ${ZMQ_INCLUDE_DIR}
  )

  if (ZMQ_LIBRARY)
    set(ZMQ_LIBRARIES
        ${ZMQ_LIBRARIES}
        ${ZMQ_LIBRARY}
    )
  endif (ZMQ_LIBRARY)

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(ZMQ DEFAULT_MSG ZMQ_LIBRARIES ZMQ_INCLUDE_DIRS)

  # show the ZMQ_INCLUDE_DIRS and ZMQ_LIBRARIES variables only in the advanced view
  mark_as_advanced(ZMQ_INCLUDE_DIRS ZMQ_LIBRARIES)

endif (ZMQ_LIBRARIES AND ZMQ_INCLUDE_DIRS)

