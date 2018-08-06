package firewall is
   type Message is record
      -- only ports for to/from for
      To : Unsigned_16; -- follows how ports are u16
      From : Unsigned_16;
   end record;

end firewall;
