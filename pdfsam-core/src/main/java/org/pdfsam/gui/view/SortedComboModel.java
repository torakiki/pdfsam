/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/lug/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.view;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.AbstractListModel;
import javax.swing.ComboBoxModel;

/**
 * Combo box data model where elements are sorted according to a comparator
 * 
 * @author Andrea Vacondio
 * @param <E>
 *            elements type
 */
public class SortedComboModel<E extends Comparable<? super E>> extends AbstractListModel<E> implements ComboBoxModel<E> {

    private final List<E> data = new ArrayList<>();
    private Object selected;

    public SortedComboModel(Collection<E> elements) {
        data.addAll(elements);
        Collections.sort(data);
    }

    @Override
    public int getSize() {
        return data.size();
    }

    public E getElementAt(int index) {
        if (index >= 0 && index < data.size()) {
            return data.get(index);
        }
        return null;
    }

    public void setSelectedItem(Object anObject) {
        if ((selected != null && !selected.equals(anObject)) || selected == null && anObject != null) {
            selected = anObject;
            fireContentsChanged(this, -1, -1);
        }
    }

    public Object getSelectedItem() {
        return selected;
    }

}
