CREATE DATABASE inventory_system;
USE inventory_system;

CREATE TABLE maker_list (
  id INT AUTO_INCREMENT PRIMARY KEY,
  Name VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  Pinyin VARCHAR(255)
);

CREATE TABLE item_type_data (
  id INT AUTO_INCREMENT PRIMARY KEY,
  MajorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  MajorTypeSKU VARCHAR(50),
  MinorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  MinorTypeSKU VARCHAR(50)
);

CREATE TABLE inventory (
  SKU VARCHAR(50) PRIMARY KEY,  -- SKU must be unique
  Maker VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Supplier/Maker name
  MajorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Major category
  MinorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Minor category
  ItemName VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Name of the item
  ProductCost DECIMAL(10, 2) NOT NULL DEFAULT 0.00, -- Average cost with 2 decimal places
  ShippingCost DECIMAL(10, 2) NOT NULL DEFAULT 0.00, -- Average shipping cost with 2 decimal places
  Quantity INT NOT NULL DEFAULT 0, -- Quantity in stock, default is 0
  ItemImagePath VARCHAR(255), -- Path or URL to item image
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, -- Automatically track creation time
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP -- Automatically track last update time
);

CREATE TABLE unique_items (
    UniqueID VARCHAR(36) PRIMARY KEY,             -- Unique identifier for each item
    SKU VARCHAR(50) NOT NULL,                     -- Foreign key referencing inventory table
    ProductCost DECIMAL(10, 2) NOT NULL,          -- unit product cost with 2 decimal places
    DomesticShippingCost DECIMAL(10, 2) NOT NULL, -- unit domestic shipping cost with 2 decimal places
    Status ENUM('采购', '国内入库', '国内出库', '国内售出', '美国入库', '美国售出', '退货') NOT NULL, -- status
    Defect ENUM('未知', '无瑕', '瑕疵', '修复') NOT NULL,
    PurchaseTime DATE,                        -- Timestamp for '采购'
    DomesticEntryTime DATE,                   -- Timestamp for '国内入库'
    DomesticExitTime DATE,                    -- Timestamp for '国内出库'
    DomesticSoldTime DATE,                    -- Timestamp for '国内售出'
    UsEntryTime DATE,                         -- Timestamp for '美国入库'
    UsSoldTime DATE,                          -- Timestamp for '美国售出'
    ReturnTime DATE,                          -- Timestamp for '退货'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, -- Creation timestamp
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, -- Last update timestamp
    FOREIGN KEY (SKU) REFERENCES inventory(SKU)  -- Relationship with inventory table
);


CREATE TABLE orders (
    OrderID VARCHAR(50) PRIMARY KEY,         -- 订单号，作为主键
    UsTrackingNumber1 VARCHAR(50),            -- 运单号1
    UsTrackingNumber2 VARCHAR(50),            -- 运单号2
    UsTrackingNumber3 VARCHAR(50),            -- 运单号3
    OrderImagePath VARCHAR(255),            -- 订单图片路径
    OrderNotes TEXT,                        -- 订单备注
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,  -- 创建时间
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP -- 更新时间
);

-- 在 UsTrackingNumber3 之后增加列
USE inventory_system;

ALTER TABLE orders
DROP COLUMN Platform;

ALTER TABLE orders
ADD COLUMN CustomerName VARCHAR(50) AFTER UsTrackingNumber3,

ALTER TABLE orders
ADD COLUMN Platform ENUM('Etsy', 'Shopify', 'TikTok', '其他') NOT NULL AFTER CustomerName;

DESCRIBE orders;

SELECT * FROM orders;

--


USE inventory_system;

-- 在 UsEntryTime 之后增加 Time 列
ALTER TABLE unique_items
ADD COLUMN UsCheckTime DATE AFTER UsEntryTime,
ADD COLUMN UsRelocationTime DATE AFTER UsCheckTime;

-- 在 ReturnTime 之后增加列
ALTER TABLE unique_items
ADD COLUMN IntlShippingMethod ENUM('海运', '空运') AFTER ReturnTime,
ADD COLUMN IntlAirTracking VARCHAR(50) AFTER IntlShippingMethod,
ADD COLUMN IntlSeaTracking VARCHAR(50) AFTER IntlAirTracking,
ADD COLUMN OrderID VARCHAR(50) AFTER IntlSeaTracking;

ALTER TABLE unique_items
ADD CONSTRAINT fk_orders_orderid
FOREIGN KEY (OrderID) REFERENCES orders(OrderID)
ON DELETE SET NULL;

-- 在 Defect 之后增加列
ALTER TABLE unique_items
ADD COLUMN DefectNotes VARCHAR(255) AFTER Defect;

ALTER TABLE unique_items
DROP COLUMN DefectNote;



DESCRIBE unique_items;

-- 修改 Status 列的枚举值
ALTER TABLE unique_items MODIFY COLUMN Status ENUM(
    '采购', 
    '国内入库', 
    '国内出库', 
    '国内售出', 
    '美国入库', 
    '美国售出', 
    '美国核对',
    '美国调货',
    '退货'
) NOT NULL;





