return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      { "folke/neoconf.nvim", cmd = "Neoconf", config = true },
      { "folke/neodev.nvim", opts = {} },
      { "mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
    },
    opts = {
      -- options for vim.diagnostic.config()
      diagnostics = {
        underline = true,
        update_in_insert = false,
        virtual_text = {
          spacing = 4,
          source = "if_many",
          prefix = function(diagnostic) -- uses new nvim 0.10 functionality
            local icons = require("mattr-.config").icons.diagnostics
            for d, icon in pairs(icons) do
              if diagnostic.severity == vim.diagnostic.severity[d:upper()] then
                return icon
              end
            end
          end,
        },
        severity_sort = true,
      },
      -- LSP Server Settings
      ---@type lspconfig.options
      servers = {
        jsonls = {},
        lua_ls = {
          -- mason = false, -- set to false if you don't want this server to be installed with mason
          -- Use this to add any additional keymaps
          -- for specific lsp servers
          ---@type LazyKeys[]
          -- keys = {},
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              completion = {
                callSnippet = "Replace",
              },
            },
          },
        },
      },
      -- add a hook for extra server setup
      -- return true if you don't want this server to be setup with lspconfig
      ---@type table<string, fun(server:string, opts:_.lspconfig.options):boolean?>
      setup = {
        -- example to setup with typescript.nvim
        -- tsserver = function(_, opts)
        --   require("typescript").setup({ server = opts })
        --   return true
        -- end,
        -- Specify * to use this function as a fallback for any server
        -- ["*"] = function(server, opts) end,
      },
    },
    config = function(_, opts)

      -- TODO keymaps and autoformatting?
      --
      local Util = require("mattr-.lsp.util")
      ---- setup autoformat
      --require("lazyvim.plugins.lsp.format").setup(opts)
      ---- setup formatting and keymaps
      Util.on_attach(function(client, buffer)
       require("mattr-.lsp.keymaps").on_attach(client, buffer)
      end)

      -- LSP (both the spec and neovim) supports dynamic registrations of
      -- capabilities. When that happens, we want to make sure we catch that
      -- and attach our keybindings after the regular registration neovim does
      local register_capability = vim.lsp.handlers["client/registerCapability"]

      vim.lsp.handlers["client/registerCapability"] = function(err, res, ctx)
        local ret = register_capability(err, res, ctx)
        local client_id = ctx.client_id
        ---@type lsp.Client|nil
        local client = vim.lsp.get_client_by_id(client_id)
        local buffer = vim.api.nvim_get_current_buf()
        require("mattr-.lsp.keymaps").on_attach(client, buffer)
        return ret
      end

      -- define signs for use on the sidebar
      for name, icon in pairs(require("mattr-.config").icons.diagnostics) do
        name = "DiagnosticSign" .. name
        vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
      end

      -- configure neovim's diagnostics
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))

      -- configure inlay_hint (which requires neovim 0.10 or later)
      local inlay_hint = vim.lsp.buf.inlay_hint or vim.lsp.inlay_hint
      if inlay_hint then
        Util.on_attach(function(client, buffer)
          if client.server_capabilities.inlayHintProvider then
            inlay_hint(buffer, true)
          end
        end)
      end

      -- configure reference highlights when the cursor stops
      Util.on_attach(function(client, buffer)
        require("mattr-.lsp.util").setup_highlight_local_references(client, buffer)
      end)

      -- Merge nvim-cmp's capabilities with neovim's built in capabilities
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        require("cmp_nvim_lsp").default_capabilities(),
        opts.capabilities or {}
      )

      local servers = opts.servers
      -- allow lsp servers that aren't available through mason to be configured
      local function setup(server)
        local server_opts = vim.tbl_deep_extend("force", {
          capabilities = vim.deepcopy(capabilities),
        }, servers[server] or {})

        if opts.setup[server] then
          if opts.setup[server](server, server_opts) then
            return
          end
        elseif opts.setup["*"] then
          if opts.setup["*"](server, server_opts) then
            return
          end
        end
        require("lspconfig")[server].setup(server_opts)
      end

      -- get all the servers that are available thourgh mason-lspconfig
      local have_mason, mlsp = pcall(require, "mason-lspconfig")
      local all_mslp_servers = {}
      if have_mason then
        all_mslp_servers = vim.tbl_keys(require("mason-lspconfig.mappings.server").lspconfig_to_package)
      end

      local ensure_installed = {} ---@type string[]
      for server, server_opts in pairs(servers) do
        if server_opts then
          server_opts = server_opts == true and {} or server_opts
          -- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
          if server_opts.mason == false or not vim.tbl_contains(all_mslp_servers, server) then
            setup(server)
          else
            ensure_installed[#ensure_installed + 1] = server
          end
        end
      end

      if have_mason then
        mlsp.setup({ ensure_installed = ensure_installed, handlers = { setup } })
      end

      -- Prefer denols over tsserver if it's configured
      -- TODO: Move this to a javascript specific file?
      -- if Util.lsp_get_config("denols") and Util.lsp_get_config("tsserver") then
      --   local is_deno = require("lspconfig.util").root_pattern("deno.json", "deno.jsonc")
      --   Util.lsp_disable("tsserver", is_deno)
      --   Util.lsp_disable("denols", function(root_dir)
      --     return not is_deno(root_dir)
      --   end)
      -- end
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    dependencies = { "hrsh7th/cmp-nvim-lsp" },
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, { { name = "nvim_lsp" } }))
    end,
  },


  -- cmdline tools and lsp servers
  {

    "williamboman/mason.nvim",
    cmd = "Mason",
    keys = { { "<leader>cm", "<cmd>Mason<cr>", desc = "Mason" } },
    opts = {
      ensure_installed = {
        "stylua",
        "shfmt",
        -- "flake8",
      },
    },
    config = function(_, opts)
      require("mason").setup(opts)
      local mr = require("mason-registry")
      local function ensure_installed()
        for _, tool in ipairs(opts.ensure_installed) do
          local p = mr.get_package(tool)
          if not p:is_installed() then
            p:install()
          end
        end
      end
      if mr.refresh then
        mr.refresh(ensure_installed)
      else
        ensure_installed()
      end
    end,
  },
}
