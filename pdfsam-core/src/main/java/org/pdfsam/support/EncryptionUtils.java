/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 2 gen 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support;

import static java.util.Objects.nonNull;

import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.util.Arrays;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 *
 */
public final class EncryptionUtils {
    private final static Logger LOG = LoggerFactory.getLogger(EncryptionUtils.class);

    public static final String T_KEY = "j!$CEnv#8G6_61gSYpt%0H%CVXhxDv-E8UHOHQyDIz%OFPE%YsaCoNH&+^d1G_ZevL!8MAEiQ+dERnvl_4grOQMmDQ2vhn_55FXDbLNMfs!U|$y7iA|dXef3dmf*&KOa";

    private EncryptionUtils() {
        // hide
    }

    /**
     * @param value
     * @return the encrypted version of value or null if value is null
     */
    public static String encrypt(String value) {
        try {
            if (nonNull(value)) {
                Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
                cipher.init(Cipher.ENCRYPT_MODE,
                        new SecretKeySpec(Arrays.copyOf(T_KEY.getBytes(StandardCharsets.UTF_8), 16), "AES"));
                return Base64.getEncoder().encodeToString(cipher.doFinal(value.getBytes(StandardCharsets.UTF_8)));
            }
        } catch (GeneralSecurityException e) {
            LOG.error("An error occurred while encrypting a string", e);
        }
        return null;

    }

    /**
     * @param value
     * @return the decrypted version of value of null if value is null
     */
    public static String decrypt(String value) {
        try {
            if (nonNull(value)) {
                Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
                cipher.init(Cipher.DECRYPT_MODE,
                        new SecretKeySpec(Arrays.copyOf(T_KEY.getBytes(StandardCharsets.UTF_8), 16), "AES"));
                return new String(cipher.doFinal(Base64.getDecoder().decode(value)), StandardCharsets.UTF_8);
            }
        } catch (GeneralSecurityException e) {
            LOG.error("An error occurred while decrypting a string", e);
        }

        return null;
    }

}
