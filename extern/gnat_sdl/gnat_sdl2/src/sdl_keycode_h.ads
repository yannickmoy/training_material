pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;

package SDL_keycode_h is

   SDLK_SCANCODE_MASK : constant := (2**30);  --  ..\SDL2_tmp\SDL_keycode.h:47
   --  arg-macro: function SDL_SCANCODE_TO_KEYCODE (X)
   --    return X or SDLK_SCANCODE_MASK;
   --  unsupported macro: KMOD_CTRL (KMOD_LCTRL|KMOD_RCTRL)
   --  unsupported macro: KMOD_SHIFT (KMOD_LSHIFT|KMOD_RSHIFT)
   --  unsupported macro: KMOD_ALT (KMOD_LALT|KMOD_RALT)
   --  unsupported macro: KMOD_GUI (KMOD_LGUI|KMOD_RGUI)

  --  Simple DirectMedia Layer
  --  Copyright (C) 1997-2018 Sam Lantinga <slouken@libsdl.org>
  --  This software is provided 'as-is', without any express or implied
  --  warranty.  In no event will the authors be held liable for any damages
  --  arising from the use of this software.
  --  Permission is granted to anyone to use this software for any purpose,
  --  including commercial applications, and to alter it and redistribute it
  --  freely, subject to the following restrictions:
  --  1. The origin of this software must not be misrepresented; you must not
  --     claim that you wrote the original software. If you use this software
  --     in a product, an acknowledgment in the product documentation would be
  --     appreciated but is not required.
  --  2. Altered source versions must be plainly marked as such, and must not be
  --     misrepresented as being the original software.
  --  3. This notice may not be removed or altered from any source distribution.
  -- 

  --*
  -- *  \file SDL_keycode.h
  -- *
  -- *  Defines constants which identify keyboard keys and modifiers.
  --  

  --*
  -- *  \brief The SDL virtual key representation.
  -- *
  -- *  Values of this type are used to represent keyboard keys using the current
  -- *  layout of the keyboard.  These values include Unicode values representing
  -- *  the unmodified character that would be generated by pressing the key, or
  -- *  an SDLK_* constant for those keys that do not generate characters.
  -- *
  -- *  A special exception is the number keys at the top of the keyboard which
  -- *  always map to SDLK_0...SDLK_9, regardless of layout.
  --  

   subtype SDL_Keycode is SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_keycode.h:45

  --       Skip uppercase letters
  --      

  --*
  -- * \brief Enumeration of valid key mods (possibly OR'd together).
  --  

   subtype SDL_Keymod is unsigned;
   KMOD_NONE : constant unsigned := 0;
   KMOD_LSHIFT : constant unsigned := 1;
   KMOD_RSHIFT : constant unsigned := 2;
   KMOD_LCTRL : constant unsigned := 64;
   KMOD_RCTRL : constant unsigned := 128;
   KMOD_LALT : constant unsigned := 256;
   KMOD_RALT : constant unsigned := 512;
   KMOD_LGUI : constant unsigned := 1024;
   KMOD_RGUI : constant unsigned := 2048;
   KMOD_NUM : constant unsigned := 4096;
   KMOD_CAPS : constant unsigned := 8192;
   KMOD_MODE : constant unsigned := 16384;
   KMOD_RESERVED : constant unsigned := 32768;  -- ..\SDL2_tmp\SDL_keycode.h:340

  -- vi: set ts=4 sw=4 expandtab:  
end SDL_keycode_h;
